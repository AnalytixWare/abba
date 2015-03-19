#' @export
abba.binary.loss = function(b1, g1, b2, g2) {
    abs((b1 + g1)/(b1 + b2 + g1 + g2) * (b1 + b2) - b1) + abs((b2 + 
        g2)/(b1 + b2 + g1 + g2) * (b1 + b2) - b2)
}
#' @export
abba.pearson.loss = function(b1, g1, b2, g2, ...) {
    # calculates the Pearson Chisq statistics and use that as the
    # infoloss measure
    sum1 = b1 + g1
    sum2 = b2 + g2
    suma = sum1 + sum2
    sumb = b1 + b2
    sumg = g1 + g2
    h0_bprob = sumb/suma
    h0_gprob = sumg/suma
    statistic = (sum1 * h0_bprob - b1)/(sum1 * h0_bprob) * (sum1 * 
        h0_bprob - b1) + (sum2 * h0_bprob - b2)/(sum2 * h0_bprob) * 
        (sum2 * h0_bprob - b2) + (sum1 * h0_gprob - g1)/(sum1 * 
        h0_gprob) * (sum1 * h0_gprob - g1) + (sum2 * h0_gprob - 
        g2)/(sum2 * h0_gprob) * (sum2 * h0_gprob - g2)
    return(statistic)
}
#' @export
abba.volume_loss = function(b1, g1, b2, g2, ...) {
    return(b1 + g1 + b2 + g2)
}

 
