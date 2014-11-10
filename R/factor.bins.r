#' Creates a factor.bins object
#' 
#' @param bin The bin labels
#' @param b Number of bads in the bin
#' @param g Number of goods in the bin
#' @param origbin A list that contains bin, b, and g that represents the 
#'   original unmerged bins
#' @param singular A vector boolean used to denote if the bin contain only a 
#'   single value or not; singular = TRUE means that the bin has never been 
#'   combined with another bin
#' @return An numeric.bins object
#' @export

factor.bins <- function(bin, b, g, origbin = NULL, singular = rep(TRUE, 1, length(bin)), ...) {
  if (is.null(origbin)) 
    origbin <- list(bin = bin, b = b, g = g)
  
  uq <- unique(c(length(bin), length(b), length(g), length(singular)))
  if (length(uq) != 1) 
    stop("Error: The length of bins, bad counts, good counts, or singular vector don't match")
  
  abbabin <- list(bin = bin, b = b, g = g, origbin = origbin, 
                  singular = singular)
  
  attr(abbabin, "class") <- "factor.bins"
  return(abbabin)
} 


