library(dplyr)
library(httr)
library(lubridate)

CGI_YAHOO <- "https://query1.finance.yahoo.com/v7/finance/download/%s?"

concat <- function(...) {
    paste(..., sep = "", collapse = "")
}

clean_names_vec <- function(x) {
    x %>%
        gsub("[^0-9A-Za-z]+", "_", .) %>%
        trimws("both", "_") %>%
        gsub("^(\\d)", "x\\1", .) %>%
        tolower()
}

clean_names <- function(x) {
    names(x) <- names(x) %>%
        clean_names_vec()
    x
}

as_unix_time <- function(date, origin = "1970-01-01") {
    date %>%
        as.POSIXct("UTC", origin = origin) %>%
        as.numeric()
}

api_kwargs <- function(kwargs) {
    paste(names(kwargs), kwargs, sep = "=", collapse = "&")
}

api_format <- function(cgi, kwargs) {
    concat(cgi, api_kwargs(kwargs))
}

api_yahoo <- function(symbol, interval = "1d", events = "history", date0 = "1900-01-01", date1 = today()) {
    kwargs <- c(period1 = as_unix_time(date0),
                period2 = as_unix_time(date1),
                interval = interval,
                events = events)

    CGI_YAHOO %>%
        sprintf(symbol) %>%
        api_format(kwargs)
}

get_yahoo <- function(url) {
    url %>%
        GET() %>%
        content(na = "null") %>%
        clean_names() %>%
        arrange(desc(date))
}

yahoo <- function(symbol, interval = "1d", events = "history", date0 = "1900-01-01", date1 = lubridate::today() + 1) {
    df <- symbol %>%
        api_yahoo(interval, events, date0, date1) %>%
        get_yahoo()

    data.frame(symbol) %>%
        cbind(df) %>%
        as_tibble()
}

spx <- yahoo("^SPX") %>%
    mutate(time = year(date) + yday(date) / (365 + leap_year(date)))

x <- spx$time
y <- spx$close

prm <- list(m = 0,
            b = 0)

opt <- optim(prm, function(prm) {
    prm <- as.list(prm)
    y_hat <- with(prm, m * x + b)
    sum((log(y) - y_hat) ^ 2)
})

opt$par
