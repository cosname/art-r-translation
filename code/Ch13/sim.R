# perform nreps repetitions of the marble experiment, to estimate 
# P(pick blue from Urn 2)
sim1 <- function(nreps)  {
   nb1 <- 10  # 10 blue marbles in Urn 1
   n1 <- 18  # number of marbles in Urn 1 at 1st pick
   n2 <- 13  # number of marbles in Urn 2 at 2nd pick
   count <- 0  # number of repetitions in which get blue from Urn 2
   for (i in 1:nreps)  {
      nb2 <- 6  # 6 blue marbles orig. in Urn 2
      # pick from Urn 1 and put in Urn 2; is it blue?
      if (runif(1) < nb1/n1) nb2 <- nb2 + 1
      # pick from Urn 2; is it blue?
      if (runif(1) < nb2/n2) count <- count + 1
   }
   return(count/nreps)  # est. P(pick blue from Urn 2)
}

sim2 <- function(nreps)  {
   nb1 <- 10  
   nb2 <- 6  
   n1 <- 18 
   n2 <- 13
   # pre-generate all our random numbers, one row per repetition
   u <- matrix(c(runif(2*nreps)),nrow=nreps,ncol=2)
   # define simfun for use in apply(); simulates one repetition
   simfun <- function(rw) {
      # rw ("row") is a pair of random numbers
      # choose from Urn 1
      if (rw[1] < nb1/n1) nb2 <- nb2 + 1
      # choose from Urn 2, and return boolean on choosing blue 
      return (rw[2] < nb2/n2) 
   }
   z <- apply(u,1,simfun)
   # z is a vector of booleans but they can be treated as 1s, 0s
   return(mean(z))  
}

sim3 <- function(nreps)  {
   nb1 <- 10  
   nb2 <- 6  
   n1 <- 18 
   n2 <- 13
   u <- matrix(c(runif(2*nreps)),nrow=nreps,ncol=2)
   # set up the condition vector 
   cndtn <- u[,1] <= nb1/n1 & u[,2] <= (nb2+1)/n2 |
            u[,1] > nb1/n1 & u[,2] <= nb2/n2 
   return(mean(cndtn))  
}

