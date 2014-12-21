#' Creates a numeric.bins object from raw values
#' 
#' @param raw The list of numeric values to bin
#' @param target A boolean factor or a list of factors. By default the bad is TRUE, however the user can set the abd
#' @return An numeric.bins object
#' @export

numeric.bins.from.raw <- function(raw, target, bin.rules = pretty(raw), bad = TRUE, ...) {
  .numeric.bins.from.raw(raw, target == bad, bin.rules, ...)
}

#' Creates a numeric.bins object from raw values assuming the target are set to
#' boolean and bin.rules are set correctly
#' 
#' @param raw The list of numeric values to bin
#' @param target A boolean factor where bad is TRUE
#' @return An numeric.bins object
#' @export
.numeric.bins.from.raw <- function(raw, target, bin.rules, ....) {
  bin <- cut(raw, breaks = bin.rules, ...)
  tbl <- table(bin, target)
  b <- tbl[,2]
  g <- tbl[,1]
  numeric.bins(bin, b = b, g = g)
}