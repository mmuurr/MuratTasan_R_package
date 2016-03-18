## Very similar in principle to Hadley's plyr::mapvalues with different behavor for missing values.
## `x` should be an atomic vector (NOTE: maybe relax this?).
## Usage when `to` is a list: `mapValues(x, foo, bar, list(replacement_val))` (i.e. make `unmapped` a length-one list).
mapValues <- function(x, from, to, unmapped = NA) {
    if(length(from) != length(to)) stop("`from` and `to` vectors are not the same length.")
    if(!is.atomic(x)) stop("`x` must be an atomic vector.")
    if(!is.null(unmapped)) {
        stopifnot(is.atomic(to) && is.atomic(unmapped) || is.list(to) && is.list(unmapped))
    }
    stopifnot(length(unmapped) == 1)

    if(is.factor(x)) {
        levels(x) <- mapValues(levels(x), from, to, unmapped)
        return(x)
    }

    from_to_iix <- match(x, from) ## length(from_to_iix) == length(x)
    unfound_x_lix <- is.na(from_to_iix) ## length(unfound_x_lix) == length(x)
    RV <- to[from_to_iix] ## length(RV) == length(x)
    if(is.null(unmapped)) {
        RV[unfound_x_lix] <- x[unfound_x_lix]
    } else {
        RV[unfound_x_lix] <- unmapped
    }
    RV
} 

