abba.merge_bins = function(bin, b, g, bin1_pos, bin2_pos, allow_intersect = F, 
    verbose = F, ...) {
    if (length(intersect(bin1_pos, bin2_pos)) > 0) {
        if (allow_intersect) {
            l = length(bin1_pos)
            for (i in 1:l) {
                p = bin1_pos[i]
                q = bin2_pos[i]
                
                b[q] = b[p] + b[q]
                g[q] = g[p] + g[q]
            }
        } else {
            print(paste(bin1_pos, bin2_pos))
            stop("Intersected bin positions not allowed")
        }
    } else {
        p = bin1_pos
        q = bin2_pos
        if (verbose) {
            print(paste("Combined (", b[p], g[p], ") with (", 
                b[q], g[q], ")"), sep = "")
            print(paste("At pos:", p, q))
        }
        b[q] = b[p] + b[q]
        g[q] = g[p] + g[q]
    }
    
    return(list(bin = bin[-bin1_pos], b = b[-bin1_pos], g = g[-bin1_pos]))
} 
