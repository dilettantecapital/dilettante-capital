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


here("gold.html") %>%
    read_quik() %>%
    write_csv(here("gold.csv"))

here("equity.html") %>%
    read_quik() %>%
    write_csv(here("equity.csv"))
