# DES application:  M/M/1 queue, arrival rate 0.5, service rate 1.0

# the call 
# dosim(mm1initglbls,mm1reactevnt,mm1prntrslts,10000.0,
#    list(arrvrate=0.5,srvrate=1.0))
# should return a value of about 2 (may take a while)

# initializes global variables specific to this app
mm1initglbls <- function(apppars) {
   mm1glbls <<- list()
   # simulation parameters
   mm1glbls$arrvrate <<- apppars$arrvrate
   mm1glbls$srvrate <<- apppars$srvrate
   # server queue, consisting of arrival times of queued jobs
   mm1glbls$srvq <<- vector(length=0) 
   # statistics
   mm1glbls$njobsdone <<- 0  # jobs done so far
   mm1glbls$totwait <<- 0.0  # total wait time so far
   # set up first event, an arrival; the application-specific data for
   # each event will consist of its arrival time, which we need to
   # record in order to later calculate the job's residence time in the
   # system
   arrvtime <- rexp(1,mm1glbls$arrvrate)
   schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
}

# application-specific event processing function called by dosim()
# in the general DES library 
mm1reactevnt <- function(head) {
   if (head$evnttype == "arrv") {  # arrival
      # if server free, start service, else add to queue (added to queue
      # even if empty, for convenience)
      if (length(mm1glbls$srvq) == 0) {
         mm1glbls$srvq <<- head$arrvtime
         srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
         schedevnt(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
      } else mm1glbls$srvq <<- c(mm1glbls$srvq,head$arrvtime)
      # generate next arrival
      arrvtime <- sim$currtime + rexp(1,mm1glbls$arrvrate)
      schedevnt(arrvtime,"arrv",list(arrvtime=arrvtime))
   } else {  # service done
      # process job that just finished
      # do accounting
      mm1glbls$njobsdone <<- mm1glbls$njobsdone + 1
      mm1glbls$totwait <<- 
         mm1glbls$totwait + sim$currtime - head$arrvtime
      # remove from queue
      mm1glbls$srvq <<- mm1glbls$srvq[-1]
      # more still in the queue?
      if (length(mm1glbls$srvq) > 0) {
         # schedule new service
         srvdonetime <- sim$currtime + rexp(1,mm1glbls$srvrate)
         schedevnt(srvdonetime,"srvdone",list(arrvtime=mm1glbls$srvq[1]))
      }
   }
}

mm1prntrslts <- function() {
   print("mean wait:")
   print(mm1glbls$totwait/mm1glbls$njobsdone)
}
