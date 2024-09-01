source("lib/library.r")


# FUNCTIONS


percent_change <- function(a, b) {
    100 * (b - a) / a
}

unrowname <- function(df) {
    rownames(df) <- NULL
    df
}

squak <- function(f) {
    function(x, ...) {
        print(x)
        f(x, ...)
    }
}

add_hlc3 <- function(df) {
    df %>%
        mutate(hlc3 = (high + low + close) / 3)
}

add_mom <- function(df) {
    df %>%
        mutate(mom = log(hlc3 / lead(hlc3)))
}

filter_month <- function(df, year, month) {
    df %>%
        filter(year(date) == year,
               month(date) == month)
}

omit_na <- function(df) {
    df %>%
        na.omit()
}

monthly <- function(sym, year, month) {
    lst <- sym %>%
        lapply(squak(yahoo)) %>%
        setNames(sym)

    lst_mon <- lst %>%
        lapply(add_hlc3) %>%
        lapply(add_mom) %>%
        lapply(filter_month, year, month)

    lst_calc <- lst_mon %>%
        lapply(function(df) {
            open <- df %>%
                filter(date == min(date)) %>%
                pull(open)

            close <- df %>%
                filter(date == max(date)) %>%
                pull(close)

            change <- percent_change(open, close)

            snr <- df %>%
                pull(mom) %>%
                na.omit() %>%
                (function(x) mean(x) / sd(x))

            data.frame(sym = df$symbol[1],
                       open = round(open, 2),
                       close = round(close, 2),
                       change = round(close - open, 2),
                       percent = round((close - open) / open, 3),
                       snr = round(snr, 2))
        })

    lst_calc %>%
        do.call(rbind, .) %>%
        unrowname()
}


# COMMON


df <- read_csv("data/csv/common.csv")

df_calc <- monthly(df$sym, 2024, 8)

df %>%
    left_join(df_calc) %>%
    select(name, sym, open, close, change, percent, snr) %>%
    arrange(-snr) %>%
    write_csv(here("digest.csv"))


# PICKING


wil <- read_csv("data/csv/wilshire.csv") %>%
    setNames(tolower(names(.))) %>%
    rename(symbol = ticker) %>%
    mutate(symbol = gsub("\\.", "-", symbol))

sym <- wil %>%
    pull(symbol) %>%
    unique()

str(sym)

# for (x in sym) {
#     print(x)
#     tryCatch({
#         yahoo(x) %>%
#             write_csv(glue("data/bulk/{x}.csv"), progress = FALSE)
#     }, error = function(e) {})
# }

dpath <- "data/bulk/"
fpath <- paste0(dpath, list.files(dpath))

str(fpath)

lst <- lapply(fpath, vroom::vroom, progress = FALSE, show_col_types = FALSE)
names(lst) <- gsub("\\.csv", "", list.files(dpath))

lst_mon <- lst %>%
    lapply(add_hlc3) %>%
    lapply(add_mom) %>%
    lapply(filter_month, 2024, 8)

# df_mon <- lst_mon[[1]]
# table(sapply(lst_mon, nrow))

lst_calc <- lst_mon %>%
    "["(21 <= sapply(., nrow)) %>%
    lapply(function(df_mon) {
        open <- df_mon %>%
            filter(date == min(date)) %>%
            pull(open)

        close <- df_mon %>%
            filter(date == max(date)) %>%
            pull(close)

        change <- percent_change(open, close)

        snr <- df_mon %>%
            pull(mom) %>%
            na.omit() %>%
            (function(x) mean(x) / sd(x))

        flow <- sum(df_mon$close * df_mon$volume)

        data.frame(sym = df_mon$symbol[1],
                   open = round(open, 2),
                   close = round(close, 2),
                   change = round(close - open, 2),
                   percent = round((close - open) / open, 3),
                   snr = round(snr, 2),
                   flow = round(flow))
    })

spx_calc <- lst_calc %>%
    do.call(rbind, .) %>%
    unrowname() %>%
    as_tibble() %>%
    arrange(-flow)

spx_calc %>%
    write_csv(here("wilshire.csv"))

spx_calc %>%
    filter(median(flow, na.rm = TRUE) <= flow) %>%
    left_join(wil %>% select(name = company, sym = symbol)) %>%
    select(name, sym, open, close, change, percent, snr) %>%
    arrange(-snr) %>%
    write_csv(here("picking.csv"))

spx_calc %>%
    with({
        pdf(here("flow.pdf"), 9, 6)
        op <- par(mar = c(5.75, 5, 5, 1.5))

        plot(density(log10(na.omit(flow))),
             main = "Wilshire 5000, August 2024\nDensity of Gross Flows",
             xlab = "Flow (Logarithm Base 10)", ylab = "Density")

        par(op)
        dev.off()
    })
