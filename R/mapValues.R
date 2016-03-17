## Basically a drop-in replacement for Hadley's plyr::mapvalues with different behavor for missing values
## If `missing_from` is `NULL` and `x` contains values not found in `from`, those unfound values remain unchanged (apart from possible coersion rules to the mode of `to`).
## If `missing_from` is non-`NULL` and `x` contains values not found in `from`, those unfound values are replaced with `missing_from`.
mapValues <- function(x, from, to, missing_from = NA) {
    if(length(from) != length(to)) stop("`from` and `to` vectors are not the same length.")
    if(!is.atomic(x)) stop("`x` must be an atomic vector.")
    if(length(missing_from) != 1) stop("`missing_from` must be a scalar.")

    if(is.factor(x)) {
        levels(x) <- mapValues(levels(x), from, to, missing_from)
        return(x)
    }

    from_to_iix <- match(x, from) ## length(from_to_iix) == length(x)
    unfound_x_lix <- is.na(from_to_iix) ## length(unfound_x_lix) == length(x)
    RV <- to[from_to_iix] ## length(RV) == length(x)
    if(!is.null(missing_from)) {
        RV[unfound_x_lix] <- missing_from
    } else { ## missing_from == NULL and thus pass unmapped `x` values through to the result
        RV[unfound_x_lix] <- x[unfound_x_lix]
    }
    RV
} 

