isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}
