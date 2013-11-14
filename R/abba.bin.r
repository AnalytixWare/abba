
# creates an abba.bin object
# the abba.bin class is a list with following characteristics
#	bin 		- 	the bin labels
#	b		- 	the number of bads in the bin
#	g		-	the number of goods in the bin
#	origbin	-	a list that contains bin, b, and g that represents the original unmerged bins
#	singular	-	used to represent if the the bin a singular or not; singular =TRUE means that the bin has
#				never been combined with another bin


abba.bin = function(bin,b,g,origbin =NULL,singular = rep(TRUE,1,length(bin)),...)
{
  if (is.null(origbin ))
    origbin = list(bin=bin,b=b,g=g)

  uq =  unique(c(
    length(bin),
    length(b),
    length(g),
    length(singular))
  )
if (length(uq) != 1) stop("Something's wrong with the bins. The number of bins, bad counts, good counts, or singular vector don't match")

  abbabin = list(bin=bin,b=b,g=g,origbin =origbin ,singular=singular)
  
  attr(abbabin,'class') = "abba.bin"
  return (abbabin)  
}