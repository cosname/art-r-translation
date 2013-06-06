emax <- function(nreps) {
   x <- rnorm(2*nreps)
   maxxy <- pmax(x[1:nreps],x[(nreps+1):(2*nreps)])
   return(mean(maxxy))
}

