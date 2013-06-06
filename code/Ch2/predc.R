predc <- function(x,k) {
   n <- length(x)
   k2 <- k/2
   # the vector red will contain our predicted values
   pred <- vector(length=n-k)
   csx <- c(0,cumsum(x))
   for (i in 1:(n-k)) {
      if (csx[i+k] - csx[i] >= k2) pred[i] <- 1 else pred[i] <- 0
   }
   return(mean(abs(pred-x[(k+1):n])))
}
