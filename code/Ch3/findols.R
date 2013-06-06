findols <- function(x) {
   findol <- function(xrow) {
       mdn <- median(xrow)
       devs <- abs(xrow-mdn)  _label~getdevs@
       return(which.max(devs))  _label~getmaxdevs@
   }
   return(apply(x,1,findol))  _label~applyfindol@
}
