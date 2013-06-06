# findud() converts vector v to 1s, 0s, representing an element
# increasing or not, relative to the previous one; output length is 1
# less than input
findud <- function(v) {
   vud <- v[-1] - v[-length(v)] _label~findvud1@
   return(ifelse(vud > 0,1,-1))  _label~findvud2@
}

udcorr <- function(x,y) {
   ud <- lapply(list(x,y),findud)
   return(mean(ud[[1]] == ud[[2]]))
}
