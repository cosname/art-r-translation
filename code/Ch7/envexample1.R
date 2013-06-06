f <- function(y) {
   d <- 8
   h <- function() {
      return(d*(w+y))
   }
   print(environment(h))
   return(h())
}
