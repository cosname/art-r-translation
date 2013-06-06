minda <- function(d) {
   smallest <- min(d)
   ij <- which(d == smallest,arr.ind=TRUE)
   return(c(smallest,ij))
}
