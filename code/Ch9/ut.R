# class "ut", compact storage of upper-triangular matrices

# utility function, returns 1+...+i
sum1toi <- function(i) return(i*(i+1)/2)

# create an object of class "ut" from the full matrix inmat (0s included)
ut <- function(inmat) {
   n <- nrow(inmat)
   rtrn <- list()  # start to build the object  
   class(rtrn) <- "ut"
   rtrn$mat <- vector(length=sum1toi(n))  
   rtrn$ix <- sum1toi(0:(n-1)) + 1  
   for (i in 1:n) {  
      # store column i
      ixi <- rtrn$ix[i]
      rtrn$mat[ixi:(ixi+i-1)] <- inmat[1:i,i]
   }
   return(rtrn)
}

# uncompress utmat to a full matrix
expandut <- function(utmat) {
   n <- length(utmat$ix)  # numbers of rows and cols of matrix
   fullmat <- matrix(nrow=n,ncol=n)
   for (j in 1:n) {
      # fill j-th column
      start <- utmat$ix[j]  
      fin <- start + j - 1 
      abovediagj <- utmat$mat[start:fin] # above-diag part of col j
      fullmat[,j] <- c(abovediagj,rep(0,n-j))  
   }
   return(fullmat)
}

# print matrix
print.ut <- function(utmat) 
   print(expandut(utmat))

# multiply one ut matrix by another, returning another ut instance;
# implement as a binary operation
"%mut%" <- function(utmat1,utmat2) {
   n <- length(utmat1$ix)  # numbers of rows and cols of matrix
   utprod <- ut(matrix(0,nrow=n,ncol=n))  
   for (i in 1:n) {  # compute col i of product
      # let a[j] and bj denote columns j of utmat1 and utmat2, respectively, 
      # so that, e.g. b2[1] means element 1 of colument 2 of utmat2 
      # then column i of product is equal to 
      #    bi[1]*a[1] + ... + bi[i]*a[i]
      # find index of start of column i in utmat2
      startbi <- utmat2$ix[i] 
      # initialize vector that will become bi[1]*a[1] + ... + bi[i]*a[i]
      prodcoli <- rep(0,i)
      for (j in 1:i) {  # find bi[j]*a[j], add to prodcoli  
         startaj <- utmat1$ix[j] 
         bielement <- utmat2$mat[startbi+j-1]
         prodcoli[1:j] <- prodcoli[1:j] + 
            bielement * utmat1$mat[startaj:(startaj+j-1)]
      }
      # now need to tack on the lower 0s
      startprodcoli <- sum1toi(i-1)+1
      utprod$mat[startbi:(startbi+i-1)] <- prodcoli
   }
   return(utprod)
}

test <- function() {
   utm1 <- ut(rbind(1:2,c(0,2)))
   utm2 <- ut(rbind(3:2,c(0,1)))
   utp <- utm1 %mut% utm2
   print(utm1)
   print(utm2)
   print(utp)
   utm1 <- ut(rbind(1:3,0:2,c(0,0,5)))
   utm2 <- ut(rbind(4:2,0:2,c(0,0,1)))
   utp <- utm1 %mut% utm2
   print(utm1)
   print(utm2)
   print(utp)
}

