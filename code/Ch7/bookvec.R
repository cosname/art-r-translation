# class "bookvec" of vectors that count writes of their elements

# each instance of the class consists of a list whose components are the
# vector values and a vector of counts

# construct a new object of class bookvec
newbookvec <- function(x) {
   tmp <- list()
   tmp$vec <- x  # the vector itself
   tmp$wrts <- rep(0,length(x))  # counts of the writes, one for each element
   class(tmp) <- "bookvec"
   return(tmp)
}

# function to read
"[.bookvec" <- function(bv,subs) {
   return(bv$vec[subs])
}

# function to write
"[<-.bookvec" <- function(bv,subs,value) {
   bv$wrts[subs] <- bv$wrts[subs] + 1  # note the recycling
   bv$vec[subs] <- value
   return(bv)
}
