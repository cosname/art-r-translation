symdiff <- function(a,b) {
   sdfxy <- setdiff(x,y)
   sdfyx <- setdiff(y,x)
   return(union(sdfxy,sdfyx))
}
