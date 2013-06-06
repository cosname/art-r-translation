# DES.R:  R routines for discrete-event simulation (DES)

# each event will be represented by a data frame row consisting of the
# following components:  evnttime, the time the event is to occur;
# evnttype, a character string for the programmer-defined event type;
# optional application-specific components, e.g.
# the job's arrival time in a queuing app

# a global list named "sim" holds the events data frame, evnts, and
# current simulated time, currtime; there is also a component dbg, which
# indicates debugging mode

# forms a row for an event of type evntty that will occur at time
# evnttm; see comments in schedevnt() regarding appin
evntrow <- function(evnttm,evntty,appin=NULL) {
   rw <- c(list(evnttime=evnttm,evnttype=evntty),appin)
   return(as.data.frame(rw))
}

# insert event with time evnttm and type evntty into event list;
# appin is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedevnt <- function(evnttm,evntty,appin=NULL) {
   newevnt <- evntrow(evnttm,evntty,appin)
   # if the event list is empty, set it to consist of evnt and return
   if (is.null(sim$evnts)) {
      sim$evnts <<- newevnt
      return()
   }
   # otherwise, find insertion point
   inspt <- binsearch((sim$evnts)$evnttime,evnttm) 
   # now "insert," by reconstructing the data frame; we find what
   # portion of the current matrix should come before the new event and
   # what portion should come after it, then string everything together
   before <- 
      if (inspt == 1) NULL else sim$evnts[1:(inspt-1),]
   nr <- nrow(sim$evnts)
   after <- if (inspt <= nr) sim$evnts[inspt:nr,] else NULL
   sim$evnts <<- rbind(before,newevnt,after)
}

# binary search of insertion point of y in the sorted vector x; returns
# the position in x before which y should be inserted, with the value
# length(x)+1 if y is larger than x[length(x)]; could be changed to C
# code for efficiency
binsearch <- function(x,y) {
   n <- length(x)
   lo <- 1
   hi <- n
   while(lo+1 < hi) {
      mid <- floor((lo+hi)/2)
      if (y == x[mid]) return(mid)
      if (y < x[mid]) hi <- mid else lo <- mid
   }
   if (y <= x[lo]) return(lo)
   if (y < x[hi]) return(hi)
   return(hi+1)
}

# start to process next event (second half done by application
# programmer via call to reactevnt()) 
getnextevnt <- function() {
   head <- sim$evnts[1,]
   # delete head
   if (nrow(sim$evnts) == 1) {
      sim$evnts <<- NULL
   } else sim$evnts <<- sim$evnts[-1,]
   return(head)
}

# simulation body
# arguments:
#    initglbls:  application-specific initialization function; inits
#      globals to statistical totals for the app, etc.; records apppars
#      in globals; schedules the first event
#    reactevnt: application-specific event handling function, coding the
#       proper action for each type of event
#    prntrslts:  prints application-specific results, e.g. mean queue
#       wait
#    apppars:  list of application-specific parameters, e.g.
#      number of servers in a queuing app
#    maxsimtime:  simulation will be run until this simulated time 
#    dbg:  debug flag; if TRUE, sim will be printed after each event
dosim <- function(initglbls,reactevnt,prntrslts,maxsimtime,apppars=NULL,
      dbg=FALSE) {
   sim <<- list()
   sim$currtime <<- 0.0  # current simulated time
   sim$evnts <<- NULL  # events data frame
   sim$dbg <<- dbg
   initglbls(apppars)
   while(sim$currtime < maxsimtime) {  
      head <- getnextevnt()
      sim$currtime <<- head$evnttime  # update current simulated time
      reactevnt(head)  # process this event 
      if (dbg) print(sim)
   }
   prntrslts()
}
