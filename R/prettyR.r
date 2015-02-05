
# this function is used to make the initial bins look nice and rounded */

#prettyR(x[as.numeric(x.as.factor)==i],min.cnt,px[i])

prettyR = function(x,min.cnt=max(as.integer(length(x)/200,5)),cutpoint=NULL,parallel=TRUE) { #min.cnt is such that each bin must have at least this many units
  #print(cutpoint)
  require(parallel)
  if (length(unique(x))==1) return(cutpoint)
  x = x[!is.na(x)] # removes the na from the run
  px = pretty(x[!is.na(x)]);
  x.as.factor = cut(x,c(-Inf,px,Inf)) ;
  t.px = table(x.as.factor);
  
  break.further.locations = which(t.px>min.cnt);
  
  # if after breaking all bins are left with min.cnt or less than no need to break any more 
  if (all(t.px[t.px !=0]<min.cnt)) { 
    return(cutpoint)    
  }
  else if (length(break.further.locations) ==0 | length(unique(x))==1 ) 
  {
    return(px)
  } else {	 
	 if (parallel) {
     #ZJ: exports foreach and doParallel instead
	   res <- foreach(i = break.further.locations,.combine=c,.export=c("prettyR"), .packages=c("foreach","doParallel") ) %dopar% prettyR(x[as.numeric(x.as.factor)==i],min.cnt,px[i])
    #res <- foreach(i = break.further.locations,.combine=c,.export=c("prettyR","%dopar%","foreach"), .packages="foreach" ) %dopar% prettyR(x[as.numeric(x.as.factor)==i],min.cnt,px[i])
   } else {
     #ZJ: exports foreach and doParallel instead
     res <- foreach(i = break.further.locations,.combine=c,.export=c("prettyR"), .packages=c("foreach","doParallel") ) %do% prettyR(x[as.numeric(x.as.factor)==i],min.cnt,px[i],FALSE)
		#res <- foreach(i = break.further.locations,.combine=c,.export=c("prettyR","%do%","foreach") ) %do% prettyR(x[as.numeric(x.as.factor)==i],min.cnt,px[i],FALSE)
	 }
    return(sort(unique(res)))
  }
}


#t1 = system.time(res.all <- lapply(da2,prettyR))
#cl = makeCluster(8)
#registerDoSNOW(cl)
#getDoParWorkers()
#t2 =system.time(res.all <- lapply(woe,prettyR))
#stopCluster(cl)

#x=woe$MIN_MTHLY_BAL_L12M
