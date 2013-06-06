aba <- read.csv("abalone.data",header=T)  
abamf <- aba[aba$Gender != "I",]  # exclude infants from the analysis
lftn <- function(clmn) {
   glm(abamf$Gender ~ clmn, family=binomial)$coef  
}
loall <- sapply(abamf[,-1],lftn)  
