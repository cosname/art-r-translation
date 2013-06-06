# orders the output of findwords() by word frequency
freqwl <- function(wrdlst) {
   freqs <- sapply(wrdlst,length)  # get word frequencies  _label~gwf@
   return(wrdlst[order(freqs)])
}
