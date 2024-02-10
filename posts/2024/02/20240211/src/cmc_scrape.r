source("lib/library.r")


# FUNCTIONS


clean_text <- function(x) {
    x %>%
        gsub("[^ -~]", "", .) %>%
        trimws()
}

parse_number <- function(x) {
    x %>%
        gsub("[^0-9.TBM]", "", .) %>%
        gsub("T$", "e12", .) %>%
        gsub("B$", "e9", .) %>%
        gsub("M$", "e6", .) %>%
        as.numeric(.)
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
        html_node("table")

    if (is.na(tab)) {
        return(NULL)
    }

    tab <- tab %>%
        html_table() %>%
        setNames(c("year", "item", "change"))

    tab %>%
        mutate(ticker) %>%
        select(ticker, year, item)
}


# SETUP


root <- "https://companiesmarketcap.com"
url <- "https://companiesmarketcap.com/usa/largest-companies-in-the-usa-by-market-cap"

urls <- paste0("https://companiesmarketcap.com/usa/largest-companies-in-the-usa-by-market-cap/?page=", 1:5)

ary <- urls %>%
    lapply(read_html) %>%
    lapply(html_nodes, "a[href$='/marketcap/']")

ticker <- ary %>%
    lapply(html_node, "div.company-code") %>%
    lapply(html_text) %>%
    unlist()

href <- ary %>%
    lapply(html_attr, "href") %>%
    unlist() %>%
    paste0(root, .) %>%
    gsub("marketcap/", "", .)

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


ary <- htmls_cap %>%
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
    })

df_info <- ary[sapply(ary, ncol) == 7] %>%
    do.call(rbind, .) %>%
    mutate(categories = NA) %>%
    rbind(do.call(rbind, ary[sapply(ary, ncol) == 8])) %>%
    arrange(parse_number(rank))

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
    "["(!(df_info$ticker %in% c("SCHW"))) %>%
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
               item = parse_number(item))
}

df_cap <- df_cap_raw %>%
    format_item() %>%
    rename(cap = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_rev <- df_rev_raw %>%
    format_item() %>%
    rename(rev = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_inc <- df_inc_raw %>%
    format_item() %>%
    rename(inc = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_ast <- df_ast_raw %>%
    format_item() %>%
    rename(ast = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_lia <- df_lia_raw %>%
    format_item() %>%
    rename(lia = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_dbt <- df_dbt_raw %>%
    format_item() %>%
    rename(dbt = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_csh <- df_csh_raw %>%
    format_item() %>%
    rename(csh = item) %>%
    filter(!duplicated(paste(ticker, year)))

df_full <- df_cap %>%
    left_join(df_rev, c("ticker", "year")) %>%
    left_join(df_inc, c("ticker", "year")) %>%
    left_join(df_ast, c("ticker", "year")) %>%
    left_join(df_lia, c("ticker", "year")) %>%
    left_join(df_dbt, c("ticker", "year")) %>%
    left_join(df_csh, c("ticker", "year"))


# EXPORTING


df_info %>%
    write_csv(here("../data/csv/cmc_info.csv"))

df_full %>%
    write_csv(here("../data/csv/cmc_full.csv"))
