#' Creates a bin rules object object from raw values
#' 
#' @param raw The list of numeric values to bin
#' @param target A boolean factor or a list of factors. By default the bad is TRUE, however the user can set the abd
#' @param g Number of goods in the bin
#' @param origbin A list that contains bin, b, and g that represents the 
#'   original unmerged bins
#' @param singular A vector boolean used to denote if the bin contain only a 
#'   single value or not; singular = TRUE means that the bin has never been 
#'   combined with another bin
#' @return An numeric.bins object
#' @export


bin.rules.numeric.bins <- function(breaks,... ) {
  breaks
}

gen_bin_labels <- function(breaks,... ) {
  levels(cut(0, breaks = breaks,...))
}