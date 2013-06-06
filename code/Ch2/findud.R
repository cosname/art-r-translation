# findud() converts vector v to 1s, 0s, representing an element
# increasing or not, relative to the previous one; output length is 1
# less than input
findud <- function(v) {
   vud <- v[-1] - v[-length(v)]
   return(ifelse(vud > 0,1,-1))
}

udcorr <- function(x,y) {
   ud <- lapply(list(x,y),findud)
   return(mean(ud[[1]] == ud[[2]]))
}
