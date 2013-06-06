findpi1 <- function(p) {
   n <- nrow(p)
   imp <- diag(n) - t(p)
   imp[n,] <- rep(1,n)
   rhs <- c(rep(0,n-1),1)
   pivec <- solve(imp,rhs)
   return(pivec)
}
