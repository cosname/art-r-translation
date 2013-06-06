# finds the cells in table tbl with the k highest frequencies; handling
# of ties is unrefined
tabdom <- function(tbl,k) {
   # create a data frame representation of tbl, adding a Freq column
   tbldf <- as.data.frame(tbl)
   # determine the proper positions of the frequencies in a sorted order
   freqord <- order(tbldf$Freq,decreasing=TRUE)  (*@ \label{yyy3} @*)
   # rearrange the data frame in that order, and take the first k rows
   dom <- tbldf[freqord,][1:k,]
   return(dom)
}
