
abba.pearson_reduce = function(abbabin,signif = 0.5,start = 1) {
  a = abbabin
  singular = a$singular
  origbin = a$origbin
  # Test of homogeneity
  l = length(a$b)
  if (start > l - 1) return(a)
  for (i in start:(l-1)) {
    chi_table = matrix(c(a$b[i],a$g[i],a$b[i+1],a$g[i+1]),ncol=2)

    if (chisq.test(chi_table)$p.value >= signif) {
      a = abba.merge_bins(a$bin,a$b,a$g,i,i+1)
      singular = singular[-i]
      singular[i] = F
      return(abba.pearson_reduce(abba.bin(a$bin,a$b,a$g,origbin=origbin,singular=singular),
        signif,start = i))
    }
  }
  return (abba.bin(a$bin,a$b,a$g,origbin,singular))
}
