# limits the number of bins
abba.max_num_bins_focus = function(abbabin,max_num_bins=20,...) {
  if (length(abbabin$b) > max_num_bins) {
    return(seq(1,length(abbabin$b)-1))	
  }
  else return(NULL)
}

abba.trendup_focus = function(abbabin,trendup=T,...) {
  if (trendup) 
    c = which(diff(abbabin$b/abbabin$g) < 0)
  else if (!trendup)
    c = which(diff(abbabin$b/abbabin$g) > 0)
  return(c) 
}

# returns an index that tells the algorithm where to focus its efforts

abba.turning_pt_pearson_min_pop_bad_focus = function(abbabin,signif,min_pop,min_bad,...) {
  #print(length(abbabin$bin))
  
  # see where the min bad or pop are not satisfied
  a = abba.pearson_min_pop_bad_focus(abbabin,signif=signif,min_pop=min_pop,min_bad=min_bad,...)

  if (length(a) > 0) return(a)
  
  # don't try to go any further if there are not enough bins
  if (length(abbabin$b) <=2) {
    return(NULL)
  }
  
  d = diff(abbabin$b/abbabin$g)
  s = sign(d)
  # if there is one and only one turning pt  
  if (sum(abs(diff(s)) == 2) == 1) {
    return(NULL)
  }
  else
    return(1:(length(abbabin$bin)-1))  
}

abba.turning_pt_pearson_min_pop_bad_min_woe_diff_focus = function(...) {
    a = abba.turning_pt_pearson_min_pop_bad_focus(...)
	b = abba.min_woe_diff_focus(...)
	return(unique(union(a,b)))
}

abba.turning_pt_pearson_min_pop_bad_min_woe_diff_max_num_bins_focus = function(...) {
    a = abba.turning_pt_pearson_min_pop_bad_focus(...)
	b = abba.min_woe_diff_focus(...)	
	r = unique(union(a,b))
    if ( length(r) == 0) {
	  c = abba.max_num_bins_focus(...)
	  return(c)	  
    }
    else {
      return(r)
    }
}

abba.min_bad_focus = function(abbabin,min_bad = sum(abbabin$b)/20,...) {
  # if a proportion is passed convert that proportion to actual numbers
  if (min_bad < 1) {
    min_bad = sum(abbabin$b)*min_bad
  }
  
  a = which(abbabin$b <= min_bad)

  if (length(a) == 0) return(NULL)

  if (a[length(a)] == length(abbabin$b)) a[length(a)] = length(abbabin$b) - 1

  return(unique(a))
}


abba.min_pop_focus = function(abbabin,min_pop = sum(abbabin$b+abbabin$g)/20,...) {
  if (min_pop < 1) {
    min_pop = sum(abbabin$b+abbabin$g)*min_pop
  }
  
  a = which((abbabin$b+abbabin$g) <= min_pop)

  if (length(a) == 0) return(NULL)

  if (a[length(a)] == length(abbabin$b)) a[length(a)] = length(abbabin$b) - 1

  return(unique(a))
}

abba.min_pop_bad_focus = function(abbabin,min_pop = sum(abbabin$b+abbabin$g)/20,min_bad = sum(abbabin$b)/20,...) {
  a = abba.min_pop_focus(abbabin,min_pop)
  b = abba.min_bad_focus(abbabin,min_bad)
  return(intersect(a,b))
}

abba.pearson_focus = function(abbabin,signif = 0.5,...) {
 # every two bin must be have a p-value of at least signif (default is 0.5) 
  # so that they judged to be sufficiently different
  
  l = length(abbabin$b)
  b1 = abbabin$b[-l]
  g1 = abbabin$g[-l]

  b2 = abbabin$b[-1]
  g2 = abbabin$g[-1]

  # if statistics is small then the two bins are likely to be the same
  statistics = abba.pearson.loss(b1,g1,b2,g2)
  

  q = qchisq(signif,1)
  
  which(statistics <= q ) 
  #which(abs(diff(abbabin$b/abbabin$g))<(2^(0.15)-1)/60)
}

abba.pearson_trendup_focus = function(abbabin,signif=0.5,trendup=T,...) {
  a=abba.pearson_focus(abbabin,signif=signif,...)
  b=abba.trendup_focus(abbabin,trendup=trendup,...)
  return(union(a,b))
}

abba.pearson_min_pop_bad_focus = function(abbabin,signif = 0.5,min_pop = sum(abbabin$b+abbabin$g)/20,min_bad = sum(abbabin$b)/20,...) {
  a = abba.min_pop_bad_focus(abbabin,min_pop=min_pop,min_bad=min_bad,...)
  b = abba.pearson_focus(abbabin,signif =signif,...)
  #print(signif)
  return(unique(union(a,b)))
}

abba.pearson_min_pop_bad_min_woe_diff_focus = function(...) {
  a = abba.pearson_min_pop_bad_focus(...)
  b = abba.min_woe_diff_focus(...)  
  return(unique(union(a,b)))
}

abba.min_woe_diff_focus = function(abbabin,min_woe_diff = 0,...) {
  # calculate the weight of evidence
  woe = log(abbabin$g/abbabin$b) -  log(sum(abbabin$g)/sum(abbabin$b))
  # calculate the weight of evidence difference
  woe_diff = abs(diff(woe))
  # return the set of indices where the weight of evidence is less than the minimum
  return(which(woe_diff < min_woe_diff))
}

abba.trendup_pearson_min_pop_bad_focus = function(...) {
  a = abba.min_pop_bad_focus(...)
  b = abba.pearson_focus(...)
  c = abba.trendup_focus(...)  
  return(unique(union(union(a,b),c)))
}

abba.trendup_pearson_min_pop_bad_min_woe_diff_max_num_bins_focus = function(...) {
  a = abba.trendup_pearson_min_pop_bad_focus(...)
  b = abba.min_woe_diff_focus(...)  
  r = unique(union(a,b))
  if (length(r)==0) {      
	  c = abba.max_num_bins_focus(...)
	  #print (paste("length0",length(c)))
	  #print(c)
	  return(c)	  
  } else {
    #print (paste("length1",length(r)))
    return(r)	
  }
}
  

abba.trendup_min_bad_focus = function(abbabin,min_bad = sum(abbabin$b)/20,trendup=T,...) {
  a = abba.min_bad_focus(abbabin,min_bad)

  if (trendup) 
    c = which(diff(abbabin$b/abbabin$g) < 0)
  else if (!trendup)
    c = which(diff(abbabin$b/abbabin$g) > 0)
  return(union(a,c)) 
}

abba.trendup_min_pop_focus = function(abbabin,min_pop = sum(abbabin$b+abbabin$g)/20,trendup=T,...) {
  a = abba.min_bad_focus(abbabin,min_pop)
  if (trendup) 
    c = which(diff(abbabin$b/abbabin$g) < 0)
  else if (!trendup)
    c = which(diff(abbabin$b/abbabin$g) > 0)
  return(union(a,c)) 
}

abba.trendup_min_pop_bad_focus = function(abbabin,min_bad = sum(abbabin$b)/20,min_pop = sum(abbabin$b+abbabin$g)/20,trendup=T,...) {
  a = abba.min_bad_focus(abbabin,min_bad)
  d = abba.min_pop_focus(abbabin,min_pop)
    

  if (trendup) 
    c = which(diff(abbabin$b/abbabin$g) < 0)
  else if (!trendup)
    c = which(diff(abbabin$b/abbabin$g) > 0)
  return(union(c,intersect(a,d))) 
}