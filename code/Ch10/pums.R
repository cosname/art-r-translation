# reads in PUMS file pf, extracting the Person records, returning a data
# frame; each row of the output will consist of the Household serial
# number and the fields specified in the list flds; the columns of
# the data frame will have the names of the indices in flds

extractpums <- function(pf,flds) {
   dtf <- data.frame()  # data frame to be built
   con <- file(pf,"r")  # connection
   # process the input file
   repeat {  
      hrec <- readLines(con,1)  # read Household record
      if (length(hrec) == 0) break  # end of file, leave loop 
      # get household serial number
      serno <- intextract(hrec,c(2,8))  
      # how many Person records?
      npr <- intextract(hrec,c(106,107))  
      if (npr > 0)
         for (i in 1:npr) {  
            prec <- readLines(con,1)  # get Person record
            # make this person's row for the data frame
            person <- makerow(serno,prec,flds)  
            # add it to the data frame
            dtf <- rbind(dtf,person)  
         }
   }
   return(dtf)
}

# set up this person's row for the data frame
makerow <- function(srn,pr,fl) {
   l <- list()
   l[["serno"]] <- srn
   for (nm in names(fl)) {
      l[[nm]] <- intextract(pr,fl[[nm]])
   }
   return(l)
}

# extracts an integer field in the string s, in character positions
# rng[1] through rng[2]
intextract <- function(s,rng) {  
   fld <- substr(s,rng[1],rng[2])
   return(as.integer(fld))  
}
