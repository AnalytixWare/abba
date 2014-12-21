#' Merge two bins in numeric.bins object
#' 
#' @param numeric.bins The numeric.bins object
#' @param bin1_pos A vector of the positions to merge
#' @param bin2_pos A vector of the positions to merge with bin1_pos
#' @param allow_intersect Allow bin positions to be in the intersect of
#'   bin1_pos; bin2_pos
#' @return An numeric.bins object
#' @export

merge.numeric.bins = function(numeric.bins, bin1_pos, bin2_pos, allow_intersect = F, 
                           verbose = F, ...) {
  
  if (length(intersect(bin1_pos, bin2_pos)) > 0) {
    if (allow_intersect) {
      l = length(bin1_pos)
      for (i in 1:l) {
        p = bin1_pos[i]
        q = bin2_pos[i]
        
        b[q] = b[p] + b[q]
        g[q] = g[p] + g[q]
      }
    } else {
      print(paste(bin1_pos, bin2_pos))
      stop("Intersected bin positions not allowed")
    }
  } else {
    p = bin1_pos
    q = bin2_pos
    if (verbose) {
      print(paste("Combined (", b[p], g[p], ") with (", 
                  b[q], g[q], ")"), sep = "")
      print(paste("At pos:", p, q))
    }
    b[q] = b[p] + b[q]
    g[q] = g[p] + g[q]
  }
  
  numeric.bins$bin <- 
  return(list(bin = bin[-bin1_pos], b = b[-bin1_pos], g = g[-bin1_pos]))
} 

