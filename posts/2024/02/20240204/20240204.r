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

parse_table <- function(html) {
    ticker <- html %>%
        html_node("div.company-code") %>%
        html_text() %>%
        clean_text()

    tab <- html %>%
        html_node("table") %>%
        html_table() %>%
        setNames(c("year", "item", "change"))

    tab %>%
        mutate(ticker) %>%
        select(ticker, year, item)
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

df_info %>%
    filter(ticker == "DHR")

df_cap_raw <- htmls_cap %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_rev_raw <- htmls_rev %>%
    lapply(parse_table) %>%
    do.call(rbind, .)


# TRANSFORMATION


df_cap <- df_cap_raw %>%
    rename(cap = item) %>%
    mutate(year = as.integer(gsub("[^0-9]", "", year)),
           cap = gsub("\\$", "", cap),
           cap = gsub(" T", "e12", cap),
           cap = gsub(" B", "e9", cap),
           cap = as.numeric(cap))

df_rev <- df_rev_raw %>%
    rename(rev = item) %>%
    mutate(year = as.integer(gsub("[^0-9]", "", year)),
           rev = gsub("\\$", "", rev),
           rev = gsub(" T", "e12", rev),
           rev = gsub(" B", "e9", rev),
           rev = gsub(" M", "e6", rev),
           rev = as.numeric(rev))

df_ps <- df_cap %>%
    left_join(df_rev) %>%
    mutate(ps = cap / rev,
           lg_ps = log(ps)) %>%
    na.omit()

df_yr_cap <- df_ps %>%
    group_by(year) %>%
    summarize(cap = mean(cap)) %>%
    arrange(-year)

df_yr <- df_ps %>%
    group_by(year) %>%
    summarize(lg_ps_cap = mean(cap * lg_ps)) %>%
    arrange(-year) %>%
    left_join(df_yr_cap) %>%
    mutate(lg_ps = lg_ps_cap / cap,
           ps = exp(lg_ps))


# PLOTTING


df_yr %>%
    with({
        pdf(here("ps_ratio.pdf"), 9, 6)
        op <- par(mar = c(5.5, 5, 3.5, 2))

        plot(year, ps, type = "n",
             main = "Cap-Weighted P/S of Current 100 Largest US Companies",
             xlab = "", ylab = "Price-to-Sales",
             xaxt = "n")
        axis(1, 2000:2030, las = 2)
        abline(h = 1:6, v = 2000:2030, col = "lightblue")
        lines(year, ps, lwd = 2)

        plot(year, lg_ps, type = "l")

        par(op)
        dev.off()
    })
