# shows the values of the local variables (including arguments) of the
# frame upn frames above the one from which showframe() is called; if
# upn < 0, the globals are shown; function objects are not shown
showframe <- function(upn) {
   # determine the proper environment
   if (upn < 0) {
      env <- .GlobalEnv
   } else {
      env <- parent.frame(n=upn+1)
   }
   # get the list of variable names
   vars <- ls(envir=env)
   # for each variable name, print its value
   for (vr in vars) {
      vrg <- get(vr,envir=env)
      if (!is.function(vrg)) {
         cat(vr,":\n",sep="")
         print(vrg)
      }
   }
}

g <- function(aa) {
   b <- 2
   showframe(0)
   showframe(1)
   aab <- h(aa+b)
   return(aab)
}
