testsuffix <- function(fn,suff) {
   parts <- strsplit(fn,".",fixed=TRUE)
   nparts <- length(parts[[1]])
   return(parts[[1]][nparts] == suff)
}
