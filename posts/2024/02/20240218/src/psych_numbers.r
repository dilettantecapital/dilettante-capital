source("lib/library.r")


# SETUP


spx <- yahoo("^SPX")
dji <- yahoo("^DJI")
ixic <- yahoo("^IXIC")
rut <- yahoo("^RUT")


# ANY NUMBER


real_any <- spx %>%
    pull(close) %>%
    round(2) %>%
    paste() %>%
    gsub("[^0-9]", "", .) %>%
    strsplit(NULL) %>%
    unlist() %>%
    table() %>%
    "/"(sum(.) / 100) %>%
    round(1)

theo_any <- spx %>%
    pull(close) %>%
    range() %>%
    log() %>%
    (function(ab) seq(ab[1], ab[2], length.out = nrow(spx))) %>%
    exp() %>%
    round(2) %>%
    paste() %>%
    gsub("[^0-9]", "", .) %>%
    strsplit(NULL) %>%
    unlist() %>%
    table() %>%
    "/"(sum(.) / 100) %>%
    round(1)

real_any
theo_any
real_any - theo_any


# FIRST NUMBER


real_fir <- spx %>%
    pull(close) %>%
    round(2) %>%
    paste() %>%
    substr(1, 1) %>%
    table() %>%
    "/"(sum(.) / 100) %>%
    round(1)

theo_fir <- spx %>%
    pull(close) %>%
    range() %>%
    log() %>%
    (function(ab) seq(ab[1], ab[2], length.out = nrow(spx))) %>%
    exp() %>%
    round(2) %>%
    paste() %>%
    substr(1, 1) %>%
    table() %>%
    "/"(sum(.) / 100) %>%
    round(1)

real_fir
theo_fir
real_fir - theo_fir


# VISUALIZATION


{
    pdf(here("../data/pdf/psych.pdf"), 9, 6)

    plot(real_any)

    plot(0:9, as.numeric((real_any - theo_any)), pch = 4)
    abline(h = 0, col = "red")

    plot(1:9, as.numeric((real_fir - theo_fir)), pch = 4)
    abline(h = 0, col = "red")

    dev.off()
}

n <- exp(runif(100, 0, 10))

floor(n / 10 ^ (log10(n) - log10(n) %% 1))
