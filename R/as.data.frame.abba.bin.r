as.data.frame.abba.bin = function(abbabin,...) {
  return(data.frame(bin = abbabin$bin,b=abbabin$b,g =abbabin$g,...))
}