findwords <- function(tf) {
   # read in the words from the file, into a vector of mode character
   txt <- scan(tf,"")  _label~scantxt@
   wl <- list()  _label~initlist@
   for (i in 1:length(txt)) {
      wrd <- txt[i]  # i-th word in input file
      wl[[wrd]] <- c(wl[[wrd]],i)  _label~catwrd@
   }   _label~endlp@
   return(wl)
}
