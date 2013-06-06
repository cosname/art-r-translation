# snow version of mutual links problem

mtl <- function(ichunk,m) {
   n <- ncol(m)
   matches <- 0
   for (i in ichunk) {
      if (i < n) {
         rowi <- m[i,]
         matches <- matches +
            sum(m[(i+1):n,] %*% rowi)
      }
   }
   matches
}

mutlinks <- function(cls,m) {
n <- nrow(m)
nc <- length(cls)
# determine which worker gets which chunk of i
options(warn=-1)
ichunks <- split(1:n,1:nc)
options(warn=0)
counts <- clusterApply(cls,ichunks,mtl,m)
do.call(sum,counts) / (n*(n-1)/2)
}

