ssnyt <- freqwl(nyt)
nwords <- length(ssnyt)
freqs9 <- sapply(ssnyt[round(0.9*nwords):nwords],length)
barplot(freqs9)
