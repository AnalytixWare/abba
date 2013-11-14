# expects the abbabin to be an abba.bin object
abba.collapse_zero_bads_or_goods_bins = function(abbabin,...) {  
  # where do the zero counts lie
  
  #w <- which(abbabin$g !=0 & abbabin$b != 0)
  w <- which( abbabin$b != 0)  
  wl <- length(w)  
   
   if(wl == 1) {
   	abbabin$b <- sum(abbabin$b)
   	abbabin$g <- sum(abbabin$g)
   	abbabin$bin <- abbabin$bin[w]
   	return(abbabin)
   }
   
  cumb <- cumsum(abbabin$b)
  cumg <- cumsum(abbabin$g)
  l <- length(cumb)
  maxb <- cumb[l]
  maxg <- cumg[l]
  
  cumb <- cumb[w]
  cumg <- cumg[w]
  
  abbabin$b <- c(cumb[1],diff(cumb)[-(wl-1)],maxb)
  abbabin$g <- c(cumg[1],diff(cumg)[-(wl-1)],maxg)
  abbabin$bin <- c(abbabin$bin[w[-wl]],abbabin$bin[l])
    
  w <- which( abbabin$g != 0)  
  wl <- length(w)  
  
  if(wl == 1) {
  	abbabin$b <- sum(abbabin$b)
  	abbabin$g <- sum(abbabin$g)
  	abbabin$bin <- abbabin$bin[w]
  	return(abbabin)
  }
  
  cumb <- cumsum(abbabin$b)
  cumg <- cumsum(abbabin$g)
  l <- length(cumb)
  maxb <- cumb[l]
  maxg <- cumg[l]
  
  cumb <- cumb[w]
  cumg <- cumg[w]
  
  abbabin$b <- c(cumb[1],diff(cumb)[-(wl-1)],maxb)
  abbabin$g <- c(cumg[1],diff(cumg)[-(wl-1)],maxg)
  abbabin$bin <- c(abbabin$bin[w[-wl]],abbabin$bin[l])
  
  return(abbabin)
} 