source("lib/library.r")


# SETUP


df_soup <- here("soup.csv") %>%
    read_csv()

df_spx <- yahoo("^SPX") %>%
    mutate(time = year(date) + yday(date) / (365 + leap_year(date)))


# VISUALIZATION


df_soup %>%
    with({
        pdf(here("soup.pdf"), 9, 6)
        op <- par(mar = c(5.75, 5, 5, 1.5))

        plot(dt, price, type = "n", log = "y",
             main = "Price History of Campbell's\nCondensed Tomato Soup",
             xlab = "Date", ylab = "Price ($)")
        abline(h = (0:20) / 10, v = 10 * (180:230), col = "lightblue")
        lines(dt, price, lwd = 2)

        spx <- approxfun(df_spx$time, df_spx$close)(dt) / price

        plot(dt, spx, type = "n", log = "y",
             main = "Price of the S&P 500 Cash Index in\nCans of Campbell's Tomato Soup",
             xlab = "Date", ylab = "Price (Cans of Tomato Soup)")
        abline(h = c(1, 10, 100, 1000, 1000, 2, 20, 200, 2000, 2000, 5, 50, 500, 5000, 5000),
               v = 10 * (180:230), col = "lightblue")
        lines(dt, spx, lwd = 2)

        par(op)
        dev.off()
    })
