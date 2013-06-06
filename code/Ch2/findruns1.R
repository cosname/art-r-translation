findruns1 <- function(x,k) {
   n <- length(x)
   runs <- vector(length=n)  _label~runsalloc@
   count <- 0
   for (i in 1:(n-k+1)) {
      if (all(x[i:(i+k-1)]==1)) {
         count <- count + 1
         runs[count] <- i _label~runsfill@
      }
   }
   if (count > 0) {
      runs <- runs[1:count] _label~runsredefine@
   } else runs <- NULL
   return(runs)
}
