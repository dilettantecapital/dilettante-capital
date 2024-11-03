source("lib/library.r")


# SETUP


jsn <- here("sp500tr.json") %>%
    jsonlite::read_json() %>%
    "$"("chart") %>%
    "$"("result") %>%
    "[["(1)

date <- unlist(jsn$timestamp) %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
    as.Date()

close <- unlist(jsn$indicators$quote[[1]]$close)

df <- data.frame(date, close) %>%
    arrange(date)


# MEAN ANNUAL RETURN


df %>%
    mutate(lg_ratio = log(close / lag(close, 12))) %>%
    pull(lg_ratio) %>%
    mean(na.rm = TRUE) %>%
    exp() %>%
    "*"(100) %>%
    "-"(100)

df %>%
    mutate(pct = (close - lag(close, 12)) / lag(close, 12)) %>%
    pull(pct) %>%
    mean(na.rm = TRUE) %>%
    "*"(100)


# VISUALIZATION


df %>%
    mutate(ratio = close / lag(close, 12)) %>%
    na.omit() %>%
    pull(date) %>%
    range()
    with({
        pdf(here("sp500tr.pdf"), 9, 6)
        op <- par(mar = c(5.75, 5, 4, 1.5))

        plot(date, ratio, type = "n", log = "y",
             main = "S&P 500 Total Return\nAnnual Performance Ratio",
             xlab = "Date", ylab = "Ratio")

        abline(h = 1,
               col = "red")
        abline(h = exp(mean(log(ratio))),
               col = "blue")
        abline(h = setdiff(seq(0, 2, 0.1), 1),
               v = ym(paste(seq(1980, 2030, 5), 1)),
               col = "lightgrey")

        lines(date, ratio)

        par(op)
        dev.off()
    })
