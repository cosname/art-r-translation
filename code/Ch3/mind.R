# returns the minimum value of d[i,j], i != j, and the row/col attaining
# that minimum, for square symmetric matrix d; no special policy on ties
mind <- function(d) {
   n <- nrow(d)
   # add a column to identify row number for apply()
   dd <- cbind(d,1:n)  _label~ddline@
   wmins <- apply(dd[-n,],1,imin)  _label~dapp@
   # wmins will be 2xn, 1st row being indices and 2nd being values
   i <- which.min(wmins[2,]) _label~xxx1@
   j <- wmins[1,i]  _label~xxx2@
   return(c(d[i,j],i,j))  _label~xxx3@
}

# finds the location, value of the minimum in a row x
imin <- function(x) {  _label~imin@
   lx <- length(x)
   i <- x[lx]  # original row number
   j <- which.min(x[(i+1):(lx-1)])  _label~wmx@
   k <- i+j  _label~iplusj@
   return(c(k,x[k]))
}
