cate.binning <- function(data, bins.list, return.factors = FALSE, split = " ", name = NULL) { 
  print("meh meh cate.binning")
  
  bins <- sapply(bins.list, function(x) paste0(x,collapse = split))

  levels.in <- function(levels,x) {
    x %in% levels
  }
  
  levels.in2 <- function(x) {
    a <- sapply(bins.list,levels.in,x)
    if(any(a)) {
      return(which(a)[1])
    } else { # one of the categories is missing
      return(NA)
    }
  }
  if(return.factors) {
    cut.data <- unlist(sapply(data,levels.in2))
  } else {
    cut.data <- bins[unlist(sapply(data,levels.in2))]
  }

  cut.data
}
