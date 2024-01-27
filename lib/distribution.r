# JOHNSON


djohnson <- function(x, gamma, delta, xi, lambda) {
    z <- (x - xi) / lambda
    t0 <- delta / (lambda * sqrt(2 * pi))
    t1 <- 1 / sqrt(1 + z^2)
    t2 <- exp(1) ^ (-0.5 * (gamma + delta * asinh(z))^2)
    (t0 * t1 * t2)
}

pjohnson <- function(q, gamma, delta, xi, lambda) {
    pnorm(gamma + delta * asinh((q - xi) / lambda))
}

rjohnson <- function(n, gamma, delta, xi, lambda) {
    lambda * sinh((qnorm(runif(n)) - gamma) / delta) + xi
}

par_johnson <- function(x) {
    prm <- list(gamma = 0, delta = 1.2, xi = mean(x), lambda = sd(x))
    fit <- fitdistrplus::fitdist(x, "johnson", start = prm)
    as.list(fit$estimate)
}
