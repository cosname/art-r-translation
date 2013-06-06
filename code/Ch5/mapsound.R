mapsound <- function(df,fromcol,tocol,sourceval) {
   base <- which(df[[fromcol]] == sourceval)  (*@ \label{whichbase} @*)
   basedf <- df[base,]  (*@ \label{getbasedf} @*)
   # determine which rows of basedf correspond to the various mapped
   # values
   sp <- split(basedf,basedf[[tocol]])  (*@ \label{mappedvals} @*)
   retval <- list()
   retval$counts <- sapply(sp,nrow)  (*@ \label{getcounts} @*)
   retval$images <- sp
   return(retval)
}
