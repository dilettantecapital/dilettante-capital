source("lib/library.r")


# FUNCTIONS


clean_text <- function(x) {
    x %>%
        gsub("[^ -~]", "", .) %>%
        trimws()
}

parse_info_box <- function(div) {
    div %>%
        html_nodes("div[class^='line']") %>%
        html_text() %>%
        gsub("[^ -~]", "", .) %>%
        trimws()
}


# SETUP


root <- "https://companiesmarketcap.com"
url <- "https://companiesmarketcap.com/usa/largest-companies-in-the-usa-by-market-cap"

links <- url %>%
    read_html() %>%
    html_nodes("a[href$='/marketcap/']")

href <- links %>%
    html_attr("href") %>%
    paste0(root, .) %>%
    gsub("marketcap/", "", .)

ticker <- links %>%
    html_node("div.company-code") %>%
    html_text()

df <- data.frame(ticker, href)


# SCRAPING


htmls_cap <- df %>%
    pull(href) %>%
    paste0("marketcap") %>%
    lapply(function(url) {
        print(url)
        url %>%
            read_html()
    })

htmls_rev <- df %>%
    pull(href) %>%
    paste0("revenue") %>%
    lapply(function(url) {
        print(url)
        url %>%
            read_html()
    })


# PARSING


df_info <- htmls_cap %>%
    lapply(function(html) {
        name <- html %>%
            html_node("div.company-name") %>%
            html_text() %>%
            clean_text()

        ticker <- html %>%
            html_node("div.company-code") %>%
            html_text() %>%
            clean_text()

        mat <- html %>%
            html_nodes("div.info-box") %>%
            lapply(parse_info_box) %>%
            do.call(rbind, .)

        data.frame(ticker, key = clean_names_vec(mat[,2]), val = mat[,1]) %>%
            pivot_wider(names_from = "key", values_from = "val")
    }) %>%
    do.call(rbind, .)

df_info

df_ps <- htmls %>%
    lapply(function(html) {
        ticker <- html %>%
            html_node("div.company-code") %>%
            html_text() %>%
            clean_text()

        tab <- html %>%
            html_node("table") %>%
            html_table() %>%
            setNames(c("year", "ps", "change"))

        tab %>%
            mutate(ticker) %>%
            select(ticker, year, ps) %>%
            mutate(ps = as.double(ps))
    }) %>%
    do.call(rbind, .)


# ANALYSIS


df_year <- df_ps %>%
    na.omit() %>%
    mutate(lg_ps = log(ps)) %>%
    group_by(year) %>%
    summarize(lg_ps = mean(lg_ps)) %>%
    arrange(-year) %>%
    mutate(ps = exp(lg_ps))


# PLOTTING


df_year %>%
    with({
        pdf(here("ps_ratio.pdf"), 9, 6)

        plot(year, ps, type = "l")
        plot(year, lg_ps, type = "l")

        dev.off()
    })
