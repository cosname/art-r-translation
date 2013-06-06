xprod <- function(x,y) {
   m <- rbind(rep(NA,3),x,y)
   xp <- vector(length=3)
   for (i in 1:3)
      xp[i] <- -(-1)^i * det(m[2:3,-i])
   return(xp)
}
