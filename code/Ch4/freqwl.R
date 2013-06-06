# orders the output of findwords() by word frequency
freqwl <- function(wrdlst) {
   freqs <- sapply(wrdlst,length)  # get word frequencies
   return(wrdlst[order(freqs)])
}
