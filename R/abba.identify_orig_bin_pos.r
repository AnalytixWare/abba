abba.identify_orig_bin_pos = function(abbabin, pos) {
    # given the position of a cut point (pos), this function will
    # return begin positions and end positions in the original
    # bins that when combined gives this bin
    
    # use a shorter name
    a = abbabin
    
    if (pos == 1) {
        origp1 = 1
    } else {
        origp1 = which(a$origbin$bin == a$bin[pos - 1]) + 1
    }
    origp2 = which(a$origbin$bin == a$bin[pos])
    
    return(c(origp1, origp2))
} 
