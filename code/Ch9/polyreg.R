# "polyreg," S3 class for polynomial regression in one predictor variable

# polyfit(y,x,maxdeg) fits all polynomials up to degree maxdeg; y is
# vector for response variable, x for predictor; creates an object of
# class "polyreg"
polyfit <- function(y,x,maxdeg) {
   # form powers of predictor variable, i-th power in i-th column
   pwrs <- powers(x,maxdeg)  # could use orthog polys for greater accuracy
   lmout <- list()  # start to build class
   class(lmout) <- "polyreg"  # create a new class
   for (i in 1:maxdeg) {
      lmo <- lm(y ~ pwrs[,1:i])
      # extend the lm class here, with the cross-validated predictions
      lmo$fitted.cvvalues <- lvoneout(y,pwrs[,1:i,drop=F])
      lmout[[i]] <- lmo
   }
   lmout$x <- x
   lmout$y <- y
   return(lmout)
}

# print() for an object fits of class "polyreg":  print
# cross-validated mean-squared prediction errors
print.polyreg <- function(fits) {
   maxdeg <- length(fits) - 2  
   n <- length(fits$y)
   tbl <- matrix(nrow=maxdeg,ncol=1)
   colnames(tbl) <- "MSPE"
   for (i in 1:maxdeg) {
      fi <- fits[[i]]
      errs <- fits$y - fi$fitted.cvvalues
      spe <- crossprod(errs,errs)  # sum of squared prediction errors
      tbl[i,1] <- spe/n
   }
   cat("mean squared prediction errors, by degree\n")
   print(tbl)
}

# forms matrix of powers of the vector x, through degree dg
powers <- function(x,dg) {
   pw <- matrix(x,nrow=length(x))
   prod <- x
   for (i in 2:dg) {
      prod <- prod * x
      pw <- cbind(pw,prod)
   }
   return(pw)
}

# finds cross-validated predicted values; could be made much faster via
# matrix-update methods
lvoneout <- function(y,xmat) {
   n <- length(y)
   predy <- vector(length=n)
   for (i in 1:n) {
      # regress, leaving out i-th observation
      lmo <- lm(y[-i] ~ xmat[-i,])
      betahat <- as.vector(lmo$coef)
      # the 1 accommodates the constant term
      predy[i] <- betahat %*% c(1,xmat[i,])
   }
   return(predy)
}

# polynomial function of x, coefficients cfs
poly <- function(x,cfs) {
   val <- cfs[1]
   prod <- 1
   dg <- length(cfs) - 1
   for (i in 1:dg) {
      prod <- prod * x
      val <- val + cfs[i+1] * prod
   }
}

