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
    function(x) {
        print(x)
        f(x)
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

df_calc <- monthly(df$sym, 2024, 7)

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

sym <- wil$symbol

for (x in sym) {
    print(x)
    tryCatch({
        yahoo(x) %>%
            write_csv(glue("data/bulk/{x}.csv"), progress = FALSE)
    }, error = function(e) {})
}

# lst <- sym %>%
#     lapply(squak(yahoo)) %>%
#     setNames(sym)

# lst_mon <- lst %>%
#     lapply(add_hlc3) %>%
#     lapply(add_mom) %>%
#     lapply(filter_month, 2024, 7)

# lst_calc <- lst_mon %>%
#     lapply(function(df) {
#         open <- df %>%
#             filter(date == min(date)) %>%
#             pull(open)

#         close <- df %>%
#             filter(date == max(date)) %>%
#             pull(close)

#         change <- percent_change(open, close)

#         snr <- df %>%
#             pull(mom) %>%
#             na.omit() %>%
#             (function(x) mean(x) / sd(x))

#         data.frame(sym = df$symbol[1],
#                     open = round(open, 2),
#                     close = round(close, 2),
#                     change = round(close - open, 2),
#                     percent = round((close - open) / open, 3),
#                     snr = round(snr, 2))
#     })

# spx_calc <- lst_calc %>%
#     do.call(rbind, .) %>%
#     unrowname()

# wil %>%
#     select(name = security, sym = symbol) %>%
#     left_join(spx_calc) %>%
#     select(name, sym, open, close, change, percent, snr) %>%
#     arrange(-snr) %>%
#     write_csv(here("picking.csv"))
