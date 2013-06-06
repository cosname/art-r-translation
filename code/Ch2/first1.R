first1 <- function(x) {
   for (i in 1:length(x)) {
      if (x[i] == 1) break  # break out of loop
   }
   return(i)
}
