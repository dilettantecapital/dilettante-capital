source("lib/library.r")


# NEW DATA


jsn <- "https://api.nasdaq.com/api/quote/SPX/historical?assetclass=index&fromdate=1900-01-01&limit=99999&todate=2030-01-01" %>%
    jsonlite::read_json()

nas <- jsn$data$tradesTable$rows %>%
    lapply(as.data.frame) %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    mutate(date = mdy(date),
           low = as.numeric(parse_number(low))) %>%
    filter(low != 0) %>%
    group_by(yr = year(date), mo = month(date)) %>%
    summarize(low = min(low), .groups = "keep") %>%
    ungroup() %>%
    mutate(date = ym(paste(yr, mo))) %>%
    select(date, low)


# SETUP


spx <- here("spx.csv") %>%
    read_csv() %>%
    group_by(yr = year(date), mo = month(date)) %>%
    summarize(low = min(low)) %>%
    ungroup() %>%
    mutate(date = ym(paste(yr, mo))) %>%
    select(date, low)

goo <- here("trends.csv") %>%
    read_csv() %>%
    mutate(date = my(date))

df <- nas %>%
    full_join(goo) %>%
    arrange(desc(date))


# VISUALIZATION


df %>%
    na.omit() %>%
    with({
        pdf(here("trends.pdf"), 9, 6)

        prm <- c(0, 1, 0, 1, 0, 1, 0, 1)
        for (k in 1:5) {
            opt <- optim(prm, function(prm) {
                x <- qnorm(pjohnson(log(relevance), prm[1], prm[2], prm[3], prm[4]))
                y <- qnorm(pjohnson(log(low), prm[5], prm[6], prm[7], prm[8]))
                z <- y - x
                tryCatch({
                    ks.test(lm(z ~ decimal_date(date))$residuals, "pnorm")$p.value
                }, error = function(e) {
                    Inf
                })
            })

            print(opt$value)
            prm <- opt$par
        }

        x <- qnorm(pjohnson(log(relevance), prm[1], prm[2], prm[3], prm[4]))
        y <- qnorm(pjohnson(log(low), prm[5], prm[6], prm[7], prm[8]))
        z <- y - x
        m <- lm(z ~ decimal_date(date))

        plot(date, low, type = "l", log = "y")
        plot(date, relevance, type = "l", log = "y")

        plot(date, x, type = "l")
        plot(date, y, type = "l")
        plot(date, z, type = "l")
        plot(date, m$residuals, type = "l")
        plot(date, pnorm(scale(m$residuals)), type = "l")

        p <- pnorm(scale(m$residuals))
        plot(date, low, type = "n", log = "y")
        for (i in seq(0, 1, length.out = 16))
            lines(date, ifelse(p < i, NA, low), lwd = 8, col = do.call(rgb, list(colorRamp(c("red", "green"), 1, "Lab")(i) / 255)))

        dev.off()
    })
