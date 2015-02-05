#' Creates an abba.bin object the abba.bin class is a list with following characteristics 
#' 
#' @param bin The bin labels
#' @param origbin The original unmerged bins 
#' @param b Number of bads in the bin
#' @param g Number of goods in the bin
#' @param singular Used to represent if the bin a singular or not; singular =
#'   TRUE means that the bin has never been combined with another bin
#' @export

abba.bin = function(bin, b, g, origbin = NULL, singular = rep(TRUE, 1, length(bin)), ...) {
    if (is.null(origbin)) 
        origbin = list(bin = bin, b = b, g = g)
    
    uq = unique(c(length(bin), length(b), length(g), length(singular)))
    if (length(uq) != 1) 
        stop("Something's wrong with the bins. The number of bins, bad counts, good counts, or singular vector don't match")
    
    abbabin = list(bin = bin, b = b, g = g, origbin = origbin, 
        singular = singular)
    
    attr(abbabin, "class") = "abba.bin"
    return(abbabin)
} 
