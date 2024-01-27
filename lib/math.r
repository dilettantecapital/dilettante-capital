# WEIGHTED


mean_w <- function(x, w, ...) {
    mean(x * w, ...) / mean(w, ...)
}
