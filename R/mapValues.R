## Very similar in principle to Hadley's plyr::mapvalues with different behavor for missing values.
##
## If `keep_x_when_missing_from` == TRUE, then:
##   (1) `missing_from_val` is ignored;
##   (2) For positions `iix` where `x` is not found in `from`, `to[iix] <- x[iix]`.
## If `keep_x_when_missing_from` == FALSE, then:
##   (1) If `to` is atomix, then `missing_from_val` must be a scalar;
##   (2) If `to` is a list (or object), `missing_from_val` can be anything that would otherwise fit into each position of `to`.
mapValues <- function(x, from, to, missing_from_val = NA, keep_x_when_missing_from = FALSE) {
    if(length(from) != length(to)) stop("`from` and `to` vectors are not the same length.")
    if(!is.atomic(x)) stop("`x` must be an atomic vector.")
    if(!keep_x_when_missing_from && is.atomic(to) && length(missing_from_val) != 1) stop("`missing_from_val` must be a scalar when `to` is an atomic vector")

    if(is.factor(x)) {
        levels(x) <- mapValues(levels(x), from, to, missing_from)
        return(x)
    }

    from_to_iix <- match(x, from) ## length(from_to_iix) == length(x)
    unfound_x_lix <- is.na(from_to_iix) ## length(unfound_x_lix) == length(x)
    RV <- to[from_to_iix] ## length(RV) == length(x)
    if(keep_x_when_missing_from) {
        RV[unfound_x_lix] <- x[unfound_x_lix]
    } else {
        RV[unfound_x_lix] <- missing_from_val
    }
    RV
} 

