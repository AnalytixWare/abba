#' Automatic Binary Binning Algorithm (abba) for this to work you need include 
#' the focus functions and the info loss functions Takes in an abba.bin object
#' and performs a binning
#' 
#' @param abbabin The abbabin object to bin
#' @param maxiter The maximum number of iteration. Defaults to Inf which means 
#'   there is no limit to the maximum number of iterations
#' @param infoloss_fn The information loss function used to determine the amount
#'   of infomration loss when combining bins. The results are then used to to
#'   focus on the problem bins
#' @param focus_fn Whic of the bins to focus on. The focussed on bins will be
#'   considered for merging
#' @param verbose Print out more error messages?
#' @param tryoptimise A boolean for try to run some optimisation algorithm or not. This feature has been disabledd
#' @param collapseZeroes Try to collapse bins with zeroes or not as the first step in the algorithm
#' @param ordered Whether the bin is ordinal (not yet implemented)
#' @param greedier Will attempt to combine more bins in one go (not yet implemented) 
#' @param ... Other arugment to go into the focus_fn or infoloss_fn
#' @return An abbabin object binned
#' 
#' @export
abba <- function(abbabin,
                 infoloss_fn    = abba.pearson.loss,
                 focus_fn     	= abba.trendup_pearson_min_pop_bad_min_woe_diff_max_num_bins_focus,
                 maxiter       	= Inf,		
                 verbose			  = FALSE,	
                 tryoptimise	  = FALSE,
                 collapseZeroes = TRUE,		
                 ordered  		  = TRUE,		
                 greedier		    = TRUE,										
                 ...) {
  
  # Combine all bins with 0 goods or 0 bads first
  # We must assume that the good bad odds can be calculated
  # In practice it's rare to have 0 good or 0 bad bins
  
  # origbin is used further down below, better keep it
  origbin = abbabin$origbin	
  
  if (collapseZeroes) {	
    abbabin = abba.collapse_zero_bads_or_goods_bins(abbabin)		
    abbabin$origbin = origbin
  }
  
  # If there is only one or two bins to start with just return it
  if (length(abbabin$b) <=2 ) return(abbabin)
  
  # compute the number of bins  
  l  = length(abbabin$bin)
  
  # compute initial loss using the provided infoloss function
  browser()
  print(system.time(infoloss <- infoloss_fn(abbabin$b[-l],abbabin$g[-l],abbabin$b[-1],abbabin$g[-1],...)))
  
  # decide the area to focus on first  
  print(system.time(focus <- focus_fn(abbabin,...)))
  if (length(focus)>1)
    focus = sort(focus)  
  
  # iterate through
  iter = 1
  # pos_to_merge : positions to merge - an array of integers, 
  # a number i in pos_to_merge means that the ith and (i+1)th cut points will be merged
  pos_to_merge = NULL
  
  #if the focus is not null and we have not exceeded the maximum number of iterations
  while (length(focus) > 0 && iter <= maxiter) {
    pos_to_merge = NULL
    
    #this bit might be a bit slow
    #while (length(focus) > 0) {
    ## which bins has the smallest infoloss out of all those in focus
    
    #min_n = min(infoloss[focus])	
    #new_pos_to_merge = intersect(which(infoloss==min_n),focus)[1]
    #pos_to_merge = c(pos_to_merge,new_pos_to_merge)
    
    ## do not consider these positions for merging until focus is recomputed
    #do_not_consider_pos = c(new_pos_to_merge-1,new_pos_to_merge,new_pos_to_merge+1,new_pos_to_merge+2)	
    
    ## take away the positions we don't want to consider from the focus
    #focus = setdiff(focus,do_not_consider_pos)	  	  	  
    #}
    
    # this code is much faster than the above code
    if (length(focus)>1)		
      pos_to_merge = focus[-(1:floor(length(focus)/2))*2]
    else
      pos_to_merge = focus
    
    # merge the bins
    print(paste("combining ", length(pos_to_merge), " bins")) # print some comments
    print(system.time( abbabin <-abba.merge_bins(abbabin$bin,abbabin$b,abbabin$g,pos_to_merge,pos_to_merge+1,...)))
    
    #recompute the focus
    focus =  focus_fn(abbabin,...)
    if (length(focus)>1)
      focus = sort(focus)
    
    # try to optimise?	
    if (length(focus)==0 && tryoptimise) {
      print("Optimising. Turn tryoptimise=FALSE if it takes too long")
      tmp = abba.redistribute_all(abba.bin(abbabin$bin,abbabin$b,abbabin$g,abbabin$origbin),focus_fn=focus_fn,infoloss_fn=infoloss_fn,verbose=verbose,...)
      if (!is.null(tmp) && any(tmp$bin!=abbabin$bin)) {
        abbabin = tmp
      }
      focus =  focus_fn(abbabin,...)
      if (length(focus)>1)
        focus = sort(focus)
    }
    
    # recompute the new length after merging and recompute the infoloss
    l = length(abbabin$bin)
    infoloss = infoloss_fn(abbabin$b[-l],abbabin$g[-l],abbabin$b[-1],abbabin$g[-1],...)
    
    #next iteration
    iter = iter + 1
    if (iter %% 100 == 0) {print(paste("iter:",iter))} # print which iteration it is in
  } 
  
  #put the result into abba bin form
  abbabin = abba.bin(abbabin$bin,abbabin$b,abbabin$g,origbin)
  
  #return the bins
  return(abbabin)
}