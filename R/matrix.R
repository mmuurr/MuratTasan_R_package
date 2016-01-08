mhead <- function(m, i = 6, j = i) {
    force(j)
    i <- head(seq_len(nrow(m)), i)
    j <- head(seq_len(ncol(m)), j)
    return(m[i,j])
}

mtail <- function(m, i = 6, j = i) {
    force(j)
    i <- tail(seq_len(nrow(m)), i)
    j <- tail(seq_len(ncol(m)), j)
    return(m[i,j])
}
