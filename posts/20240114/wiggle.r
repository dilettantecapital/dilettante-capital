# constants

(phi <- (1 + sqrt(5)) / 2)
(e <- exp(1))
pi

# combinations

e / phi

# wiggler

ssd <- function(a, b) {
    sum((a - b)^2)
}

optimize(function(i) {
    d0 <- ssd(i, phi)
    d1 <- ssd(i * i, e)
    d0 + d1
}, c(1, 4))

optimize(function(i) {
    d0 <- ssd(i, phi)
    d1 <- ssd(i * i, e)
    d0 + d1
}, c(1, 4))

# relations

vec <- c(0.318310,
         0.367879,
         0.515036,
         0.562500,
         0.595241,
         0.618034,
         0.750000,
         0.865256,
         1.155727,
         1.333333,
         1.618034,
         1.679991,
         1.777778,
         1.941611,
         2.718282,
         3.141593)

vec <- c(phi, e, pi)

grow <- vec
for (i in 1:20) {
    # grow <- c(2 * grow, grow, grow / 2)
    grow <- c(grow,
              phi * grow, grow / phi,
              e * grow, grow / e,
              pi * grow, grow / pi)
    grow <- unique(grow)
}

filt <- grow[1 <= grow & grow < 2]
index <- seq(1, 2, length.out = length(filt))
plot(density(log(grow), adjust = 0.1))
plot(index, log(sort(filt)) - log(index), type = "l")
