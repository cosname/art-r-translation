sim <- function(nreps) {
   commdata <- list()
   commdata$countabsamecomm <- 0
   for (rep in 1:nreps) {  
      commdata$whosleft <- 1:20  
      commdata$numabchosen <- 0  
      commdata <- choosecomm(commdata,5)
      if (commdata$numabchosen == 0) {
         commdata <- choosecomm(commdata,4)  
         if (commdata$numabchosen == 0) 
            commdata <- choosecomm(commdata,3)
      }
   }
   print(commdata$countabsamecomm/nreps)
}
