mapsound <- function(df,fromcol,tocol,sourceval) {
   base <- which(df[[fromcol]] == sourceval)
   basedf <- df[base,]
   # determine which rows of basedf correspond to the various mapped
   # values
   sp <- split(basedf,basedf[[tocol]])
   retval <- list()
   retval$counts <- sapply(sp,nrow)
   retval$images <- sp
   return(retval)
}
