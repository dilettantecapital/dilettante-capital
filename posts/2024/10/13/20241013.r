source("lib/library.r")


# FUNCTIONS


read_quik <- function(fpath) {
    key <- c("group", "date", "net", "long", "short")

    fpath %>%
        read_html() %>%
        html_nodes("area") %>%
        html_attr("fields") %>%
        strsplit("~") %>%
        "["(sapply(., length) == 5) %>%
        lapply(strsplit, "\\|") %>%
        lapply(sapply, "[", 2) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        setNames(key) %>%
        as_tibble() %>%
        mutate(date = mdy(date),
               net = as.numeric(parse_number(net)),
               long = as.numeric(parse_number(long)),
               short = as.numeric(parse_number(short)))
}


# SETUP


# here("gold.html") %>%
#     read_quik() %>%
#     write_csv(here("gold.csv"))

# here("equity.html") %>%
#     read_quik() %>%
#     write_csv(here("equity.csv"))

df_gold <- here("gold.csv") %>%
    read_csv() %>%
    mutate(group = gsub("[^a-zA-Z]", "_", tolower(group))) %>%
    pivot_wider(names_from = "group", values_from = c("net", "long", "short"), values_fn = first) %>%
    arrange(date)

df_equity <- here("equity.csv") %>%
    read_csv() %>%
    mutate(group = gsub("[^a-zA-Z]", "_", tolower(group))) %>%
    pivot_wider(names_from = "group", values_from = c("net", "long", "short")) %>%
    arrange(date)


# VISUALIZATION


{
    pdf(here("cot.pdf"), 9, 6)
    op <- par(mar = c(5.75, 5.75, 4, 1.5))

    net <- df_gold$long_managed_money - df_gold$short_managed_money + df_gold$short_swap_dealer - df_gold$long_swap_dealer
    plot(df_gold$date, net / 1000, type = "l",
         main = "Gold Positioning\nManagers vs. Dealers",
         xlab = "Date", ylab = "Net Positioning\n(Thousands of Contracts)")
    abline(h = 0, col = "red")

    net <- df_equity$long_asset_manager_institutional - df_equity$short_asset_manager_institutional + df_equity$short_dealer_intermediary - df_equity$long_dealer_intermediary
    plot(df_equity$date, net / 1000, type = "l",
         main = "U.S. Equity Index Positioning\nManagers vs. Dealers",
         xlab = "Date", ylab = "Net Positioning\n(Thousands of Contracts)")
    abline(h = 0, col = "red")

    par(op)
    dev.off()
}


# S&P 500 DATA


df_lerp <- df_equity %>%
    mutate(net = df_equity$long_asset_manager_institutional - df_equity$short_asset_manager_institutional + df_equity$short_dealer_intermediary - df_equity$long_dealer_intermediary) %>%
    select(date, net) %>%
    mutate(time = decimal_date(date))

df_spx <- "https://www.multpl.com/s-p-500-historical-prices/table/by-month" %>%
    read_html() %>%
    html_table() %>%
    "[["(1) %>%
    clean_names() %>%
    mutate(date = mdy(date),
           value = as.numeric(parse_number(value))) %>%
    mutate(time = decimal_date(date),
           net = approxfun(df_lerp$time, df_lerp$net)(time)) %>%
    na.omit()

df_spx %>%
    attach()

{
    pdf(here("spx.pdf"), 9, 6)
    op <- par(mar = c(5.75, 5, 4, 1.5))

    x <- df_spx$time
    y <- log(df_spx$value)
    z <- as.numeric(scale(df_spx$net))

    prm <- c(0.09043775, -174.62875483, 0, 1, mean(z), 1)
    # opt <- optim(prm, function(prm) {
    #     tryCatch({
    #         mb <- prm[1] * time + prm[2]
    #         res <- y - mb
    #         john <- qnorm(pjohnson(z, prm[3], prm[4], prm[5], prm[6]))
    #         out <- res - john
    #         sum(res ^ 2)
    #     }, error = function(e) {
    #         Inf
    #     })
    # })

    # prm <- opt$par
    mb <- prm[1] * time + prm[2]
    res <- y - mb
    john <- qnorm(pjohnson(z, prm[3], prm[4], prm[5], prm[6]))
    out <- res - john

    plot(x, y, type = "l")
    lines(x, mb, col = "red")

    plot(x, res, type = "l")
    plot(x, john, type = "l")
    plot(x, out, type = "l")

    par(op)
    dev.off()
}
