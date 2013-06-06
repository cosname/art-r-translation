# snow version of k-means clustering problem

library(snow)

# returns distances from x to each vector in y;
# here x is a single vector and y is a bunch of them
#
# define distance between 2 points to be the sum of the absolute values
# of their componentwise differences; e.g. distance between (5,4.2) and
# (3,5.6) is 2 + 1.4 = 3.4
dst <- function(x,y) {
tmpmat <- matrix(abs(x-y),byrow=T,ncol=length(x))  # note recycling
rowSums(tmpmat)
}

# will check this worker's mchunk matrix against currctrs, the current
# centers of the groups, returning a matrix; row j of the matrix will
# consist of the vector sum of the points in mchunk closest to j-th
# current center, and the count of such points
findnewgrps <- function(currctrs) {
ngrps <- nrow(currctrs)
spacedim <- ncol(currctrs)  # what dimension space are we in?
# set up the return matrix
sumcounts <- matrix(rep(0,ngrps*(spacedim+1)),nrow=ngrps)
for (i in 1:nrow(mchunk)) {
   dsts <- dst(mchunk[i,],t(currctrs))
   j <- which.min(dsts)
   sumcounts[j,] <- sumcounts[j,] + c(mchunk[i,],1)
}
sumcounts
}

parkm <- function(cls,m,niters,initcenters) {
   n <- nrow(m)
   spacedim <- ncol(m)  # what dimension space are we in?
   # determine which worker gets which chunk of rows of m
   options(warn=-1)
   ichunks <- split(1:n,1:length(cls))
   options(warn=0)
   # form row chunks
   mchunks <- lapply(ichunks,function(ichunk) m[ichunk,])
   mcf <- function(mchunk) mchunk <<- mchunk
   # send row chunks to workers; each chunk will be a global variable at
   # the worker, named mchunk
   invisible(clusterApply(cls,mchunks,mcf))
   # send dst() to workers
   clusterExport(cls,"dst")
   # start iterations
   centers <- initcenters
   for (i in 1:niters) {
      sumcounts <- clusterCall(cls,findnewgrps,centers)
      tmp <- Reduce("+",sumcounts)
      centers <- tmp[,1:spacedim] / tmp[,spacedim+1]
      # if a group is empty, let's set its center to 0s
      centers[is.nan(centers)] <- 0
   }
   centers
}
