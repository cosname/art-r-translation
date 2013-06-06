sumtree <- function(drtr) {
   tot <- 0
   # get names of all files in the tree
   fls <- dir(drtr,recursive=TRUE)  
   for (f in fls) {
      # is f a directory?
      f <- file.path(drtr,f)
      if (!file.info(f)$isdir) {  
         tot <- tot + sum(scan(f,quiet=TRUE))
      }
   }
   return(tot)
}
