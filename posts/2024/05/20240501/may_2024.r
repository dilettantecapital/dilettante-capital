source("lib/library.r")


# FUNCTIONS


percent_change <- function(a, b) {
    100 * (b - a) / a
}

unrowname <- function(df) {
    rownames(df) <- NULL
    df
}


# COMMON


txt <- ("^SPX | Equities | USA | S&P 500
^IXIC | Equities | USA | NASDAQ Composite
^DJI | Equities | USA | Dow Jones Industrial Average
^RUT | Equities | USA | Russell 2000
000001.SS | Equities | China | SSE Composite
^HSI | Equities | China | Hang Seng
^N225 | Equities | Japan | Nikkei 225
^FTSE | Equities | UK | FTSE 100
^NSEI | Equities | India | NIFTY 50
^GDAXI | Equities | Germany | DAX
^FCHI | Equities | France | CAC 40
^GSPTSE | Equities | Canada | S&P/TSX Composite
^AXJO | Equities | Australia | S&P/ASX 200
^VIX | Index | USA | CBOE Volatility
^TNX | Bonds | USA | 10 Year Treasury Note Yield
CL=F | Commodites | Energy | Crude Oil
GC=F | Commodites | Metals | Gold
DX-Y.NYB | Currencies | USA | US Dollar")

mat <- txt %>%
    strsplit(., "\n") %>%
    unlist() %>%
    strsplit(., " \\| ") %>%
    do.call(rbind, .)

df <- mat %>%
    as.data.frame() %>%
    setNames(c("sym", "cat", "subcat", "name"))

lst <- df %>%
    apply(1, function(row) {
        yahoo(row["sym"])
    }) %>%
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
