source("lib/library.r")


# FUNCTIONS


relerr <- function(new, old) {
    (new - old) / old
}


# SETUP


spx <- "^GSPC" %>%
    yahoo()

range(spx$date)


# TRANSFORMATION


spx_mom <- spx %>%
    mutate(hlc3 = (high + low + close) / 3,
           pct_day = 100 * relerr(close, lead(close, 1)),
           high_day = 100 * relerr(high, lead(close, 1)),
           pct_quarter = 100 * relerr(close, lead(close, 63)),
           pct_decade = 100 * relerr(close, lead(close, 2520)),
           lgp_quarter = 100 * log(close / lead(close, 63)),
           lgp_5y = 100 * log(close / lead(close, 1260)),
           lgp_decade = 100 * log(close / lead(close, 2520)))


# DISTRIBUTIONS


prm_quarter <- par_johnson(na_omit(spx_mom$lgp_quarter))
prm_5y <- par_johnson(na_omit(spx_mom$lgp_5y))

x_quarter <- na_omit(spx_mom$lgp_quarter)
x_5y <- na_omit(spx_mom$lgp_5y)

mean(x_quarter) / 63
mean(x_5y) / 1260
median(x_quarter) / 63
median(x_5y) / 1260

100 * exp(mean(x_quarter) / 100) - 100
100 * exp(mean(x_5y) / 100) - 100

100 * exp(sd(x_quarter) / 100) - 100
100 * exp(sd(x_5y) / 100) - 100

100 * mean(x_quarter < 0)
100 * mean(x_5y < 0)

mean(x_quarter) / sd(x_quarter)
mean(x_5y) / sd(x_5y)


# PLOTTING


spx_mom %>%
    with({
        pdf(here("density.pdf"), 9, 6)
        op <- par(mar = c(5.75, 5, 4, 1.5))

        x <- seq(-200, 200, 0.1)
        y <- do.call(djohnson, c(list(x = x), prm_quarter))
        plot(x, y, type = "l", col = "red", xlim = c(-75, 125),
             main = "S&P 500 Log Change Returns",
             xlab = "Log-Point Change", ylab = "Density")
        abline(v = mean_w(x, y), col = "red", lty = 2)

        y <- do.call(djohnson, c(list(x = x), prm_5y))
        lines(x, y)
        abline(v = mean_w(x, y), lty = 2)

        legend(-70, 0.06, c("5 Years", "3 Months"), col = c("black", "red"), lwd = 1)

        par(op)
        dev.off()
    })


# PLAYING


252 / 2
252 / 4
252 / 12

mean(0.4321 < spx_mom$high_day, na.rm = TRUE)
1.004321^252
