library(dplyr)
library(httr)

urls <- "archive/links.txt" %>%
    readLines()

paths <- urls %>%
    gsub(".+/", "", .) %>%
    paste0("archive/dump/", ., ".html")

resps <- urls %>%
    lapply(GET)

resps %>%
    lapply(content, as = "raw") %>%
    mapply(writeBin, ., paths)
