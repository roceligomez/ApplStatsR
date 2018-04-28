foo <- function(reps = 20, n = 1e5){
  for(r in seq_len(reps)) {
    x <- rnorm(n)
    o <- order(x)
    x <- x[o]
  }
  invisible(NULL)
}