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


# COMMON


df <- read_csv("data/csv/common.csv")

lst <- df %>%
    pull(sym) %>%
    lapply(squak(yahoo)) %>%
    setNames(df$sym)

lst["GC=F"]

df_calc <- lst %>%
    lapply(function(df) {
        df_month <- df %>%
            mutate(hlc3 = (high + low + close) / 3,
                   mom = log(hlc3 / lead(hlc3))) %>%
            filter(year(date) == year(today())) %>%
            filter(month(date) == month(today()))

        open <- df_month %>%
            filter(date == min(date)) %>%
            pull(open)

        close <- df_month %>%
            filter(date == max(date)) %>%
            pull(open)

        change <- percent_change(open, close)

        snr <- df_month %>%
            pull(mom) %>%
            na.omit() %>%
            (function(x) mean(x) / sd(x))

        data.frame(sym = df$symbol[1],
                   open = round(open, 2),
                   close = round(close, 2),
                   change = round(close - open, 2),
                   percent = round((close - open) / open, 3),
                   snr = round(snr, 2))
    }) %>%
    do.call(rbind, .) %>%
    unrowname()

df %>%
    left_join(df_calc) %>%
    select(name, sym, open, close, change, percent, snr) %>%
    arrange(-snr) %>%
    write_csv(here("digest.csv"))


# FLOWS


sym <- "^SPX"

df_month <- yahoo(sym) %>%
    filter(year(date) == year(today())) %>%
    filter(month(date) == month(today()))

first <- df_month %>%
    filter(date == min(date)) %>%
    pull(open)

last <- df_month %>%
    filter(date == max(date)) %>%
    pull(open)

percent_change(first, last)

price <- df_month %>%
    pull(close) %>%
    log() %>%
    mean() %>%
    exp()

volume <- df_month %>%
    pull(volume) %>%
    sum()

flow_gross <- df_month %>%
    mutate(flow = close * volume) %>%
    pull(flow) %>%
    sum()

flow_net <- df_month %>%
    mutate(flow = (close - open) * volume) %>%
    pull(flow) %>%
    na.omit() %>%
    sum()

flow_net <- (last - first) * volume
