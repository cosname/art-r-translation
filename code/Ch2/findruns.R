findruns <- function(x,k) {
   n <- length(x)
   runs <- NULL
   for (i in 1:(n-k+1)) {
      if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i) _label~allx@
   }
   return(runs)
}
