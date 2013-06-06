# returns the minimum value of d[i,j], i != j, and the row/col attaining
# that minimum, for square symmetric matrix d; no special policy on ties
mind <- function(d) {
   n <- nrow(d)
   # add a column to identify row number for apply()
   dd <- cbind(d,1:n)
   wmins <- apply(dd[-n,],1,imin)
   # wmins will be 2xn, 1st row being indices and 2nd being values
   i <- which.min(wmins[2,])
   j <- wmins[1,i]
   return(c(d[i,j],i,j))
}

# finds the location, value of the minimum in a row x
imin <- function(x) {
   lx <- length(x)
   i <- x[lx]  # original row number
   j <- which.min(x[(i+1):(lx-1)])
   k <- i+j
   return(c(k,x[k]))
}
