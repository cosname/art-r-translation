# merges data frames for 2 fangyans
merge2fy <- function(fy1,fy2) {
   outdf <- merge(fy1,fy2)  (*@ \label{initoutdf} @*)
   # separate tone from sound, and create new columns
   for (fy in list(fy1,fy2)) {  (*@ \label{startfor} @*)
      # saplout will be a matrix, init cons in row 1, remainders in row
      # 2, and tones in row 3
      saplout <- sapply((fy[[2]]),sepsoundtone) (*@ \label{makeso} @*)
      # convert it to a data frame
      tmpdf <- data.frame(fy[,1],t(saplout),row.names=NULL, 
         stringsAsFactors=F)  (*@ \label{maketmp} @*)
      # add names to the columns
      consname <- paste(names(fy)[[2]]," cons",sep="")
      restname <- paste(names(fy)[[2]]," sound",sep="")
      tonename <- paste(names(fy)[[2]]," tone",sep="")
      names(tmpdf) <- c("Ch char",consname,restname,tonename)
      # need to use merge(), not cbind(), due to possibly different
      # ordering of fy, outdf
      outdf <- merge(outdf,tmpdf)  (*@ \label{addfy} @*)
   }
   return(outdf)
}

# separates romanized pronunciation pronun into initial consonant, if any, 
# the remainder of the sound, and the tone, if any 
sepsoundtone <- function(pronun) {  (*@ \label{startseps} @*)
   nchr <- nchar(pronun)
   vowels <- c("a","e","i","o","u")
   # how many initial consonants?
   numcons <- 0
   for (i in 1:nchr) {  (*@ \label{getcons} @*)
      ltr <- substr(pronun,i,i)
      if (!ltr %in% vowels) numcons <- numcons + 1 else break
   }
   cons <- if (numcons > 0) substr(pronun,1,numcons) else NA
   tone <- substr(pronun,nchr,nchr)
   numtones <- tone %in% letters  # T is 1, F is 0 (*@ \label{ltrs} @*)
   if (numtones == 1) tone <- NA
   therest <- substr(pronun,numcons+1,nchr-numtones)
   return(c(cons,therest,tone))  (*@ \label{sepret} @*)
}
