# my own cut function

cut2 = function(x,low,high,sexcl,eexcl,factor.group=NULL,autocover=TRUE) {
  
  # if the length of low and high are different then quit
  #if (length(low) !=  length(high)) {
  #    print("Error: Length of low and high must be the same!")
  #    return (NA)
  #}
  
  y = rep(0,length(x))
  if (autocover==TRUE) {
    if (low[1] != -Inf) {
      low = c(-Inf,low)
      high = c(low[2],high)
      sexcl = c(TRUE,sexcl)
      eexcl = c(!sexcl[2],eexcl)
      if (!is.null(factor.group)) {
        factor.group = c(max(factor.group) + 1,factor.group)
      }
    }
    
    if (high[length(high)] != Inf) {
      low = c(low,high[length(high)])
      high = c(high,Inf)
      sexcl = c(sexcl,!eexcl[length(eexcl)])
      eexcl = c(eexcl,TRUE)
      if (!is.null(factor.group)) {
        factor.group = c(factor.group,max(factor.group) + 1)
      }
    } 
    
    for (i in 1:length(low)) {
      if (sexcl[i] & eexcl[i]) {
        y[low[i] < x & x < high[i]] = i
      }
      else if (sexcl[i] & !eexcl[i]){
        y[low[i] < x & x <= high[i]] = i
      }
      else if (!sexcl[i] & eexcl[i]) {
        y[low[i] <= x & x< high[i]] = i
      }
      else if (!sexcl[i] & !eexcl[i]) {
        y[low[i] <= x & x<= high[i]] = i 
      }
    }
    
    make.levels.label <- function(lhse) {
      bracket.left = c("(","[")[ifelse(lhse[3],1,2)]
        bracket.right = c (")","]")[ifelse(lhse[4],1,2)]
        paste(bracket.left,lhse[1],",",lhse[2],bracket.right)
      }
      
      levels.labels = apply(cbind(low,high,sexcl,eexcl),1,make.levels.label)
    #print(y)
    #print(levels.labels)
      if (!is.null(factor.group)) {
      # the factor.group is expected to be a vector of values
        for (i in unique(factor.group)) {
        #print(i)
          levels.labels[factor.group==i] = paste(levels.labels[factor.group==i],collapse="+")
        #print(factor.group==i)
        }
      }
      return (addNA(factor(levels.labels[y],ordered = FALSE),ifany=TRUE)) 
    }
  }

#cut2 tests
# l = c(0,100,200)
# h = c(100,200,300)
# x = c(50,0,100,200,300,150,250,450)
# sexcl = c(TRUE,TRUE,TRUE)
# eexcl = c(FALSE,FALSE,FALSE)
# factor.group=c(1,2,1)

# a = cut2(x,l,h,sexcl,eexcl,factor.group)
# print(a)
