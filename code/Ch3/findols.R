findols <- function(x) {
   findol <- function(xrow) {
       mdn <- median(xrow)
       devs <- abs(xrow-mdn)
       return(which.max(devs))
   }
   return(apply(x,1,findol))
}
