NA2 <- function(x, replacement) {
    x[is.na(x)] <- replacement
    x
}

Inf2 <- function(x, replacement) {
    x[is.infinite(x)] <- replacement
    x
}

NaN2 <- function(x, replacement) {
    x[is.nan(x)] <- replacement
    x
}
