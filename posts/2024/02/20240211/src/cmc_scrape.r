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

htmls_inc <- df %>%
    pull(href) %>%
    paste0("earnings") %>%
    lapply(function(url) {
        print(url)
        url %>%
            read_html()
    })

htmls_ast <- df %>%
    pull(href) %>%
    paste0("total-assets") %>%
    lapply(function(url) {
        print(url)
        url %>%
            read_html()
    })

htmls_lia <- df %>%
    pull(href) %>%
    paste0("total-liabilities") %>%
    lapply(function(url) {
        print(url)
        url %>%
            read_html()
    })

htmls_dbt <- df %>%
    pull(href) %>%
    paste0("total-debt") %>%
    lapply(function(url) {
        print(url)
        url %>%
            read_html()
    })

htmls_csh <- df %>%
    pull(href) %>%
    paste0("cash-on-hand") %>%
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

df_cap_raw <- htmls_cap %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_rev_raw <- htmls_rev %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_inc_raw <- htmls_inc %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_ast_raw <- htmls_ast %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_lia_raw <- htmls_lia %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_dbt_raw <- htmls_dbt %>%
    lapply(parse_table) %>%
    do.call(rbind, .)

df_csh_raw <- htmls_csh %>%
    "["(df_info$ticker != "SCHW") %>%
    lapply(function(html) {
        ticker <- html %>%
            html_node("div.company-code") %>%
            html_text() %>%
            clean_text()

        print(ticker)

        parse_table(html)
    }) %>%
    do.call(rbind, .)


# TRANSFORMATION


format_item <- function(df) {
    df %>%
        mutate(year = as.integer(substr(year, 1, 4)),
               item = gsub("\\$", "", item),
               item = gsub(" T", "e12", item),
               item = gsub(" B", "e9", item),
               item = gsub(" M", "e6", item),
               item = as.numeric(item))
}

df_cap <- df_cap_raw %>%
    format_item() %>%
    rename(cap = item)

df_rev <- df_rev_raw %>%
    format_item() %>%
    rename(rev = item)

df_inc <- df_inc_raw %>%
    format_item() %>%
    rename(inc = item)

df_ast <- df_ast_raw %>%
    format_item() %>%
    rename(ast = item)

df_lia <- df_lia_raw %>%
    format_item() %>%
    rename(lia = item)

df_dbt <- df_dbt_raw %>%
    format_item() %>%
    rename(dbt = item)

df_csh <- df_csh_raw %>%
    format_item() %>%
    rename(csh = item)

df_full <- df_cap %>%
    left_join(df_rev) %>%
    left_join(df_inc) %>%
    left_join(df_ast) %>%
    left_join(df_lia) %>%
    left_join(df_dbt) %>%
    left_join(df_csh)


# EXPORTING


df_info %>%
    write_csv(here("cmc_info.csv"))

df_full %>%
    write_csv(here("cmc_full.csv"))
