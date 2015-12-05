"%ni%" <- Negate("%in%")

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

chunk <- function(x, n_chunks = NULL, max_chunk_size = NULL, method = c("seq", "mod", "rand")) {
    if((is.null(n_chunks) && is.null(max_chunk_size)) ||
       (!is.null(n_chunks) && !is.null(max_chunk_size))) {
        stop("exactly one of 'n_chunks' or 'max_chunk_size' must be non-NULL")
    }
    
    method <- match.arg(method)

    if(is.null(n_chunks))
        n_chunks <- ceiling(length(x) / max_chunk_size)
    
    split_iix <- (seq_along(x) - 1) %% n_chunks
    
    if(identical(method, "seq")) {
        split_iix <- sort(split_iix)
    } else if(identical(method, "rand") && length(split_iix) > 1) {
            split_iix <- sample(split_iix)
    }
    
    split(x, split_iix)
}

chunkIix <- function(n, n_chunks = NULL, max_chunk_size = NULL, method = c("seq", "mod", "rand")) {
    chunk(x = seq_len(n), n_chunks = n_chunks, max_chunk_size = max_chunk_size, method = method)
}

## basically just to match the sample vs sample.int distinction.
chunk.int <- chunkIix
