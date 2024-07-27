source("lib/library.r")


# SETUP


spx <- yahoo("^SPX") %>%
    mutate(time = year(date) + yday(date) / (365 + leap_year(date)))

x <- spx$time
y <- spx$close

prm <- list(m = 0,
            b = 0)

opt <- optim(prm, function(prm) {
    prm <- as.list(prm)
    y_hat <- with(prm, m * x + b)
    sum((log(y) - y_hat) ^ 2)
})

opt$par

spx %>%
    select(date, time) %>%
    head() %>%
    as.data.frame()


# VISUALIZATION


{
    pdf(here("reg0.pdf"), 9, 6)
    op <- par(mar = c(5.5, 5.25, 4.25, 1.75))

    x <- spx$time
    y <- spx$close

    m <- lm(log(y) ~ x)
    y_hat <- exp(m$fitted.values)

    plot(x, y, type = "n", log = "y",
         main = "S&P 500 Cash Index\nLog-Linear Regression",
         xlab = "Year", ylab = "Price",
         las = 1)
    lines(x, y_hat, col = "red")
    lines(x, y)

    text(1930, 3000, "ln(Price) = 0.0664 * Year - 126.3",
         col = "red", pos = 4)

    print(cor(log(y), log(y_hat))^2)
    print(sqrt(mean((log(y) - log(y_hat))^2)))

    par(op)
    dev.off()
}

{
    pdf(here("reg0_resid.pdf"), 9, 6)
    op <- par(mar = c(5.5, 5.25, 3.25, 1.75))

    plot(x, log(y) - log(y_hat), type = "n",
         main = "Regression Residuals",
         xlab = "Year", ylab = "Residual",
         las = 1)
    abline(h = 0, col = "red")
    lines(x, log(y) - log(y_hat))

    par(op)
    dev.off()
}

{
    pdf(here("reg1.pdf"), 9, 6)
    op <- par(mar = c(5.5, 5.25, 4.25, 1.75))

    x <- spx$time
    y <- spx$close

    prm <- list(m = 0.0665756376214989,
                b = -126.628486910239,
                A = 0.331717514065288,
                w = 0.189895546970751,
                o = -0.596633208869366)

    opt <- optim(prm, function(prm) {
        prm <- as.list(prm)
        y_hat <- with(prm, (m * x + b) + (A * sin(w * x + o)))
        sum((log(y) - y_hat) ^ 2)
    })

    as.list(opt$par)

    y_hat <- as.list(opt$par) %>%
        with((m * x + b) + (A * sin(w * x + o))) %>%
        exp()

    plot(x, y, type = "n", log = "y",
         main = "S&P 500 Cash Index\nNonlinear Regression",
         xlab = "Year", ylab = "Price",
         las = 1)
    lines(x, y_hat, col = "red")
    lines(x, y)

    text(1930, 3000, "ln(Price) = 0.06658 * Year - 126.6 + 0.3345 * sin(0.1899 * Year - 0.593)",
         col = "red", pos = 4)

    print(cor(log(y), log(y_hat))^2)
    print(sqrt(mean((log(y) - log(y_hat))^2)))

    par(op)
    dev.off()
}

{
    pdf(here("reg1_sine.pdf"), 9, 6)
    op <- par(mar = c(5.5, 5.25, 3.25, 1.75))

    time <- seq(1920, 2040, length.out = 999)
    sine <- as.list(opt$par) %>%
        with(A * sin(w * time + o))

    plot(time, sine, type = "n",
         main = "Market Cycles",
         xlab = "Year", ylab = "Residual",
         las = 1)
    abline(v = seq(1900, 2100, 10), col = "lightgrey")
    abline(v = time[which((lag(sine) > sine) & (sine < lead(sine)))], col = "blue")
    abline(v = time[which((lag(sine) < sine) & (sine > lead(sine)))], col = "red")
    lines(time, sine)

    par(op)
    dev.off()
}

cat(paste(names(opt$par), "=", opt$par), sep = ",\n")
