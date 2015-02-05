#' @export
abba.redistribute_all = function(abbabin, verbose = F, ...) {
    redis_at_least_one = F
    l = length(abbabin$b) - 1
    if (l == 0) 
        return(abbabin)
    random_index = permute(1:l)
    for (i in random_index) {
        res = abba.redistribute(abbabin, i, ...)
        if (!is.null(res)) {
            redis_at_least_one = T
            abbabin = abba.bin(res$bin, res$b, res$g, origbin = abbabin$origbin)
        }
    }
    if (redis_at_least_one) {
        res = abba.redistribute_all(abbabin, ...)
        if (!is.null(res)) {
            return(res)
        } else return(abbabin)
    } else return(NULL)
}

#' @export
abba.redistribute = function(abbabin, pos, infoloss_fn = abba.pearson.loss, 
    focus_fn = abba.trendup_pearson_min_pop_bad_focus, verbose = F, 
    ...) {
    
    # REQUIREMENT: pos cannot be the last position!!!
    if (pos == length(abbabin$bin)) 
        stop("pos cannot be the last position!!!")
    
    # attach the abbabin for easy working
    a = abbabin
    
    # locate the cut points in the original bin first origp1_1 is
    # the first bin origp2_2 is the last bin all the bins between
    # origp1_1 and origp2_2 combines together to give the current
    # bin at position p1 similarly for origp2_1 and origp2_2
    
    # p1 is position 1 p2 is position 2 they denote position in
    # the abbabin in which the bins to redistribute are situated
    p1 = pos
    p2 = pos + 1
    
    if (p1 == 1) {
        origp1_1 = 1
    } else {
        origp1_1 = which(a$origbin$bin == a$bin[p1 - 1]) + 1
    }
    origp1_2 = which(a$origbin$bin == a$bin[p1])
    
    origp2_1 = which(a$origbin$bin == a$bin[p2 - 1]) + 1
    origp2_2 = which(a$origbin$bin == a$bin[p2])
    
    tot_info_loss = infoloss_fn(a$b[p1], a$g[p1], a$b[p2], a$g[p2], 
        ...)
    
    # best_cut is the best cutting point in terms of least info
    # loss
    init_loss = tot_info_loss
    best_cut = origp1_2
    
    # calc the adjacent bins' good bad
    
    for (i in origp1_1:(origp2_2 - 1)) {
        new_b1 = sum(a$origbin$b[origp1_1:i])
        new_b2 = sum(a$origbin$b[(i + 1):origp2_2])
        new_g1 = sum(a$origbin$g[origp1_1:i])
        new_g2 = sum(a$origbin$g[(i + 1):origp2_2])
        
        new_b = c(new_b1, new_b2)
        new_g = c(new_g1, new_g2)
        
        if (p1 != 1) {
            new_b = c(a$b[p1 - 1], new_b)
            new_g = c(a$g[p1 - 1], new_g)
        }
        
        if (p2 != length(a$bin)) {
            new_b = c(new_b, a$b[p2 + 1])
            new_g = c(new_g, a$g[p2 + 1])
        }
        
        focus = focus_fn(abba.bin(1:length(new_b), new_b, new_g), 
            ...)
        
        if (length(focus) == 0) {
            tot_info_loss1 = infoloss_fn(new_b1, new_g1, new_b2, 
                new_g2, ...)
            
            # i want to find the point of greatest differentiation so the
            # bins should look as different as possible
            if (tot_info_loss < tot_info_loss1) {
                tot_info_loss = tot_info_loss1
                best_cut = i
            }
        }
    }
    
    # now it is solved where to best redistribute. Yay!!!
    
    # if the best cut is the same as the original then return NULL
    if (best_cut == origp1_2) 
        return(NULL) else {
        a$b[p1] = sum(a$origbin$b[origp1_1:best_cut])
        a$g[p1] = sum(a$origbin$g[origp1_1:best_cut])
        a$b[p2] = sum(a$origbin$b[(best_cut + 1):origp2_2])
        a$g[p2] = sum(a$origbin$g[(best_cut + 1):origp2_2])
        a$bin[p1] = a$origbin$bin[best_cut]
        
        a$singular[p1] = (best_cut == origp1_1)
        a$singular[p2] = ((best_cut + 1) == origp2_2)
        
        if (verbose || T) 
            print(paste("redis:", init_loss, tot_info_loss))
        return(a)
    }
} 
