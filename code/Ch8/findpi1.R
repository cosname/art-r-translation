findpi1 <- function(p) {
   n <- nrow(p)
   imp <- diag(n) - t(p)  (*@ \label{imp} @*)
   imp[n,] <- rep(1,n)  (*@ \label{row1s} @*)
   rhs <- c(rep(0,n-1),1)  (*@ \label{rhs} @*)
   pivec <- solve(imp,rhs)  (*@ \label{getpi} @*)
   return(pivec)
}
