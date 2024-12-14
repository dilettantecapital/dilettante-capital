source("lib/library.r")


# FUNCTIONS


qqtad <- function(p) {
    sum(abs(p - seq(0, 1, length.out = length(p))))
}

djohnson <- function(x, gamma = 0, delta = 1, xi = 0, lambda = 1) {
    z <- (x - xi) / lambda
    scale <- delta / (lambda * sqrt(2 * pi))
    shape <- 1 / sqrt(1 + z ^ 2)
    tail <- exp(-0.5 * (gamma + delta * asinh(z)) ^ 2)
    scale * shape * tail
}

pjohnson <- function(q, gamma = 0, delta = 1, xi = 0, lambda = 1) {
    stats::pnorm(gamma + delta * asinh((q - xi) / lambda))
}

qjohnson <- function(p, gamma = 0, delta = 1, xi = 0, lambda = 1) {
    xi + lambda * sinh((stats::qnorm(p) - gamma) / delta)
}


# IHS


{
    pdf(here("asinh.pdf"), 9, 6)
    op <- par(mar = c(4, 3.5, 4, 1.5))

    x <- seq(-10, 10, length.out = 999)
    r <- setdiff((-10):10, 0)

    plot(x, log(x), type = "n", asp = 1,
         xlim = c(-2*pi, 2*pi), ylim = c(-3, 3),
         main = "Comparison Between Natural Logarithm\nand Inverse Hyperbolic Sine",
         xlab = "", ylab = "")
    abline(v = r, h = r, col = "lightgrey")
    abline(v = 0, h = 0, col = "blue")
    lines(x, log(x), col = "black", lwd = 3)
    lines(x, asinh(x), col = "red", lwd = 3)
    legend(-5.5, 2.5, bg = "white", lwd = 3,
           legend = c("log", "asinh"),
           col = c("black", "red"))

    par(op)
    dev.off()
}


# SETUP


jsn <- here("sp500tr.json") %>%
    jsonlite::read_json() %>%
    "$"("chart") %>%
    "$"("result") %>%
    "[["(1)

date <- unlist(jsn$timestamp) %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
    as.Date()

close <- unlist(jsn$indicators$quote[[1]]$close)

df <- data.frame(date, close) %>%
    arrange(date)


# MEAN ANNUAL RETURN


x <- df %>%
    mutate(lg_ratio = log(close / lag(close, 12))) %>%
    pull(lg_ratio) %>%
    na.omit()

gdxl <- c(gamma = 0, delta = 1, xi = 0, lambda = 1)
opt <- stats::optim(gdxl, function(gdxl) {
    p <- pjohnson(x, gdxl[1], gdxl[2], gdxl[3], gdxl[4])
    qqtad(p[order(x)])
})

gdxl <- opt$par
opt <- stats::optim(gdxl, function(gdxl) {
    p <- pjohnson(x, gdxl[1], gdxl[2], gdxl[3], gdxl[4])
    ks <- suppressWarnings(stats::ks.test(stats::qnorm(p), "pnorm"))
    -ks$p.value
})

gdxl <- opt$par

{
    pdf(here("sp500tr.pdf"), 9, 6)
    op <- par(mar = c(5.75, 5, 4, 1.5))

    d <- density(x)
    y <- djohnson(d$x, gdxl[1], gdxl[2], gdxl[3], gdxl[4])
    r <- setdiff((-10):10, 0)

    plot(range(d$x), range(c(d$y, y)), type = "n",
         main = "Modeling Returns Using Johnson's SU",
         xlab = "Annual Performance (Log Ratio)", ylab = "Density")
    abline(v = r/10, h = r, col = "lightgrey")
    abline(v = 0, h = 0, col = "blue")
    lines(d$x, d$y, col = "black", lwd = 3)
    lines(d$x, y, col = "red", lwd = 3)
    legend(-0.55, 3.5, bg = "white", lwd = 3,
           legend = c("Kernel", "Johnson's SU"),
           col = c("black", "red"))

    par(op)
    dev.off()
}

optimize(function(x) -djohnson(x, gdxl[1], gdxl[2], gdxl[3], gdxl[4]), c(-1, 1))
exp(0.1542313)

pjohnson(0, gdxl[1], gdxl[2], gdxl[3], gdxl[4])

pjohnson(log(1.05), gdxl[1], gdxl[2], gdxl[3], gdxl[4]) - pjohnson(log(0.95), gdxl[1], gdxl[2], gdxl[3], gdxl[4])

x <- seq(0, 2, length.out = 1e3)
d <- djohnson(x, gdxl[1], gdxl[2], gdxl[3], gdxl[4])
exp(sum(x * d) / sum(d))
