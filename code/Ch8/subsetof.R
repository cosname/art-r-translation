%subsetof%" <- function(u,v) {
   return(setequal(intersect(u,v),u))
}
