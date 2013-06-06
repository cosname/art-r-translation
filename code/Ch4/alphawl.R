# sorts wrdlst, the output of findwords() alphabetically by word
alphawl <- function(wrdlst) {
   nms <- names(wrdlst) # the words
   sn <- sort(nms)  # same words in alpha order
   return(wrdlst[sn])  # return rearranged version  _label~rearr@
}
