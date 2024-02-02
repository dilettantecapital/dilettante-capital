# CONSTANTS


CGI_YAHOO <- "https://query1.finance.yahoo.com/v7/finance/download/%s?"


# GENERAL


content <- function(x, ..., progress = FALSE, show_col_types = FALSE) {
    httr::content(x, ..., progress = progress, show_col_types = show_col_types)
}


# YAHOO


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
        httr::GET() %>%
        content(na = "null") %>%
        clean_names() %>%
        dplyr::arrange(dplyr::desc(date))
}

yahoo <- function(symbol, interval = "1d", events = "history", date0 = "1900-01-01", date1 = lubridate::today()) {
    df <- symbol %>%
        api_yahoo(interval, events, date0, date1) %>%
        get_yahoo()

    data.frame(symbol) %>%
        cbind(df) %>%
        dplyr::as_tibble()
}


