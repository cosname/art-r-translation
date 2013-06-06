subtable <- function(tbl,subnames) {
   # get array of cell counts in tbl
   tblarray <- unclass(tbl)  (*@ \label{unclasstbl} @*)
   # we'll get the subarray of cell counts corresponding to subnames by
   # calling do.call() on the "[" function; we need to build up a list
   # of arguments first
   dcargs <- list(tblarray)  (*@ \label{yyy1} @*)
   ndims <- length(subnames)  # number of dimensions 
   for (i in 1:ndims) {
      dcargs[[i+1]] <- subnames[[i]]
   }  (*@ \label{yyy11} @*)
   subarray <- do.call("[",dcargs)  (*@ \label{yyy2} @*)
   # now we'll build the new table, consisting of the subarray, the
   # numbers of levels in each dimension, and the dimnames() value, plus
   # the "table" class attribute
   dims <- lapply(subnames,length) (*@ \label{yyya} @*)
   subtbl <- array(subarray,dims,dimnames=subnames)
   class(subtbl) <- "table"
   return(subtbl)
}
