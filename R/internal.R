passShowAttributes <- function(source, target) {
  lapply(c("showQspray", "showRatioOfQsprays", "showSymbolicQspray"),
         function(a) {
           attr(target, a) <<- attr(source, a)
         })
  target
}

`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isNonnegativeInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x != 0
}
