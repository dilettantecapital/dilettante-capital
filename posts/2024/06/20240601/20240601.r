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
        filter(year(date) == 2024,
               month(date) == 5)
}

omit_na <- function(df) {
    df %>%
        na.omit()
}

monthly <- function(sym) {
    lst <- sym %>%
        lapply(squak(yahoo)) %>%
        setNames(sym)

    lst_mon <- lst %>%
        lapply(add_hlc3) %>%
        lapply(add_mom) %>%
        lapply(filter_month, 2024, 5)

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

df_calc <- monthly(df$sym)

df %>%
    left_join(df_calc) %>%
    select(name, sym, open, close, change, percent, snr) %>%
    arrange(-snr) %>%
    write_csv(here("digest.csv"))

# PICKING

ndx <- read_tsv("data/tsv/ndx.tsv")

ndx_calc <- monthly(ndx$symbol)

ndx %>%
    select(name, sym = symbol) %>%
    left_join(ndx_calc) %>%
    select(name, sym, open, close, change, percent, snr) %>%
    arrange(-snr) %>%
    write_csv(here("picking.csv"))
