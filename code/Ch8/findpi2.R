findpi2 <- function(p) {
   n <- nrow(p)
   # find first eigenvector of P transpose
   pivec <- eigen(t(p))$vectors[,1]
   # guaranteed to be real, but could be negative
   if (pivec[1] < 0) pivec <- -pivec
   # normalize to sum to 1
   pivec <- pivec / sum(pivec)
   return(pivec)
}
