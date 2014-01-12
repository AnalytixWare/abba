sourabba.breakup_all_bins = function(abbabin, ...) {
    l = length(abbabin$bin)
    done_at_least_one = F
    random_index = permute(1:l)
    i = 1
    while (!done_at_least_one && i <= l) {
        tmp = abba.breakup_bins(abbabin, random_index[i], ...)
        if (!is.null(tmp) && length(tmp$bin) > length(a$bin)) {
            abbabin = tmp
            done_at_least_one = T
        } else i = i + 1
    }
    
    if (done_at_least_one) {
        tmp = abba.breakup_all_bins(abbabin, ...)
        if (!is.null(tmp)) {
            return(tmp)
        } else return(abbabin)
    } else return(NULL)
}

abba.breakup_bins = function(abbabin, pos, focus_fn = abba.trendup_pearson_min_pop_bad_focus, 
    verbose = F, ...) {
    
    # use a shorter name for easier working
    a = abbabin
    
    # find the pos in the original bins that merged to give this
    # bin
    origpos = abba.identify_orig_bin_pos(abbabin, pos)
    # print(origpos)
    
    if (origpos[1] == origpos[2]) {
        if (verbose) {
            print("This is a singular bin. Check for logical errors")
        }
        return(NULL)
        # stop('This is a singular bin. Check for logical errors')
    }
    
    random_index = permute(origpos[1]:(origpos[2] - 1))
    for (i in random_index) {
        new_b1 = sum(a$origbin$b[origpos[1]:i])
        new_b2 = sum(a$origbin$b[(i + 1):origpos[2]])
        new_g1 = sum(a$origbin$g[origpos[1]:i])
        new_g2 = sum(a$origbin$g[(i + 1):origpos[2]])
        
        new_b = c(new_b1, new_b2)
        new_g = c(new_g1, new_g2)
        
        if (pos != 1) {
            new_b = c(a$b[pos - 1], new_b)
            new_g = c(a$g[pos - 1], new_g)
        }
        
        if (pos != length(a$bin)) {
            new_b = c(new_b, a$b[pos + 1])
            new_g = c(new_g, a$g[pos + 1])
        }
        
        focus = focus_fn(abba.bin(1:length(new_b), new_b, new_g), 
            ...)
        
        # found a breaking point
        if (length(focus) == 0) {
            if (verbose || T) {
                print(paste("break up from (", new_b1 + new_b2, 
                  new_g1 + new_g2, ") -> (", new_b1, new_g1, ") + (", 
                  new_b2, new_g2, ")"))
            }
            
            if (pos == 1) {
                a$b = c(new_b1, new_b2, a$b[(pos + 1):length(a$b)])
                a$g = c(new_g1, new_g2, a$g[(pos + 1):length(a$g)])
                a$bin = c(a$origbin$bin[i], a$origbin$bin[origpos[2]], 
                  a$bin[(pos + 1):length(a$bin)])
            } else {
                a$b = c(a$b[1:(pos - 1)], new_b1, new_b2, a$b[(pos + 
                  1):length(a$b)])
                a$g = c(a$g[1:(pos - 1)], new_g1, new_g2, a$g[(pos + 
                  1):length(a$g)])
                a$bin = c(a$bin[1:(pos - 1)], a$origbin$bin[i], 
                  a$origbin$bin[origpos[2]], a$bin[(pos + 1):length(a$bin)])
            }
            return(a)
        }
    }
    return(NULL)
} 
