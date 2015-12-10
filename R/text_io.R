timecat <- function(s, ...) {
    cat(sprintf("[%s] %s", Sys.time(), s), ...)
}


sstr <- function(object, ...) {
    sprintf("%s\n%s\n",
            sprintf("str(%s):", deparse(substitute(object))),
            paste0(capture.output(str(object, ...)), collapse = "\n"))
}
