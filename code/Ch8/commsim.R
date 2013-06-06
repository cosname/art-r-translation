sim <- function(nreps) {
   commdata <- list()  # will store all our info about the 3 committees
   commdata$countabsamecomm <- 0
   for (rep in 1:nreps) {
      commdata$whosleft <- 1:20  # who's left to choose from
      commdata$numabchosen <- 0  # number among A, B chosen so far
      # choose committee 1, and check for A,B serving together
      commdata <- choosecomm(commdata,5)
      # if A or B already chosen, no need to look at the other comms.
      if (commdata$numabchosen > 0) next  
      # choose committee 2 and check
      commdata <- choosecomm(commdata,4)
      if (commdata$numabchosen > 0) next  
      # choose committee 3 and check
      commdata <- choosecomm(commdata,3)
   }
   print(commdata$countabsamecomm/nreps)
}

choosecomm <- function(comdat,comsize) {
   # choose committee
   committee <- sample(comdat$whosleft,comsize)
   # count how many of A and B were chosen
   comdat$numabchosen <- length(intersect(1:2,committee))
   if (comdat$numabchosen == 2) 
      comdat$countabsamecomm <- comdat$countabsamecomm + 1
   # delete chosen committee from the set of people we now have to choose from
   comdat$whosleft <- setdiff(comdat$whosleft,committee)
   return(comdat)
}
