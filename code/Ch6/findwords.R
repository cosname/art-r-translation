findwords <- function(tf) {
   # read in the words from the file, into a vector of mode character
   txt <- scan(tf,"") 
   words <- split(1:length(txt),txt)
   return(words)
}
