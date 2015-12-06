##--------------------------------------------------------------------------------
## WARNING!:
## coalesceNA(...) tests each of the ... elements for being identical to NA.
## If a value is passed that is a vector _containing_ NA values, this vector itself is _not_ identical to NA.
## This function is (ideally) indented to operate on scalar inputs, not vectors.
## As such, a warning is raised if any of the ... arguments are not scalars.
## Thus, `coalesceNA(NA, c(1, NA), 2)` will return `c(1, NA)`, _not_ `2`.
## PROCEED WITH CAUTION!
##
## Also take care to consider that NA and NULL values are _not identical_.
## Thus, `coalesceNA(NULL, NA)` will return `NULL`.
##--------------------------------------------------------------------------------
coalesceNA <- function(..., default = NA) {
    the_args <- list(...)
    if(any(!sapply(the_args, is.scalar))) {
        warning("non-scalar inputs detected", immediate. = TRUE)
    }
    for(x in the_args) {
        if(!identical(NA, x)) return(x)
    }
    ## fallthrough case.
    return(default)
}

##--------------------------------------------------------------------------------
## Each arg in (...) is tested against being `NULL`.
## Take care to consider that NA and NULL values are _not identical_.
## Thus `coalesceNULL(NULL, NA)` will return `NA`.
##--------------------------------------------------------------------------------
coalesceNULL <- function(..., default = NULL) {
    for(x in list(...)) {
        if(!is.null(x)) return(x)
    }
    return(default)
}
