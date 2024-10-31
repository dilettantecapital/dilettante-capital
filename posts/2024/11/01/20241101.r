source("lib/library.r")


# FUNCTIONS


percent_change <- function(a, b) {
    100 * (b - a) / a
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

parse_html <- function(fpath) {
    sym <- fpath %>%
        stringr::str_match(".+/(.+?).html") %>%
        "["(, 2)

    fpath %>%
        read_html() %>%
        html_table() %>%
        "[["(1) %>%
        clean_names() %>%
        rename(close = close_close_price_adjusted_for_splits) %>%
        mutate(symbol = sym,
               date = mdy(date),
               open = as_number(open),
               high = as_number(high),
               low = as_number(low),
               close = as_number(close),
               volume = as_number(volume)) %>%
        select(symbol, date, open, high, low, close, volume)
}

monthly <- function(sym, year, month) {
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


# SETUP


folder <- "data/html"
fnames <- list.files(folder)
fpaths <- file.path(folder, fnames)

lst <- fpaths %>%
    lapply(parse_html)

read_csv("data/csv/common.csv") %>%
    left_join(monthly(lst, 2024, 10)) %>%
    select(name, sym, open, close, change, percent, snr) %>%
    arrange(-snr) %>%
    write_csv(here("digest.csv"))
