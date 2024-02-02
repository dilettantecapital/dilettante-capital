suppressPackageStartupMessages({
    library(rstudioapi)
    library(survival)
    library(fitdistrplus)
    library(lubridate)
    library(rvest)
    library(httr)
    library(tibble)
    library(tidyr)
    library(readr)
    library(dplyr)
})

source("lib/distribution.r")
source("lib/math.r")
source("lib/scrape.r")


# GENERAL


na_omit <- function(x) {
    as.vector(na.omit(x))
}


# STRINGS


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

concat <- function(...) {
    paste(..., sep = "", collapse = "")
}


# OPERATING SYSTEM


here <- function(...) {
    if (rstudioapi::isAvailable()) {
        home <- dirname(rstudioapi::getSourceEditorContext()$path)
    } else {
        home <- commandArgs() %>%
            tibble::enframe(name = NULL) %>%
            tidyr::separate(value, c("key", "value"), "=", fill = "right") %>%
            dplyr::filter(key == "--file") %>%
            dplyr::pull(value) %>%
            gsub("\\\\", "/", .) %>%
            gsub("/[^/]+?$", "", .)
    }

    paste(home, ..., sep = "/")
}