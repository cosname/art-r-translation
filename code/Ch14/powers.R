# forms matrix of powers of the vector x, through degree dg
powers1 <- function(x,dg) {
   pw <- matrix(x,nrow=length(x))
   prod <- x  # current product
   for (i in 2:dg) {
      prod <- prod * x
      pw <- cbind(pw,prod)
   }
   return(pw)
}

powers2 <- function(x,dg) {
   pw <- matrix(nrow=length(x),ncol=dg)
   prod <- x  # current product
   pw[,1] <- prod
   for (i in 2:dg) {
      prod <- prod * x
      pw[,i] <- prod 
   }
   return(pw)
}

powers3 <- function(x,dg) return(outer(x,1:dg,"^"))

> powers4
function(x,dg) {
   repx <- matrix(rep(x,dg),nrow=length(x))
   return(t(apply(repx,1,cumprod)))
}
> system.time(powers4(x,8))
   user  system elapsed
 28.106   1.120  83.255
\end{Code}

