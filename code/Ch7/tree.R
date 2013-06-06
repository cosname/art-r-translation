# routines to create trees and insert items into them are included
# below; a deletion routine is left to the reader as an exercise

# storage is in a matrix, say m, one row per node of the tree; if row 
# i contains (u,v,w), then node i stores the value w, and has left and
# right links to rows u and v; null links have the value NA 

# the tree is represented as a list (mat,nxt,inc), where mat is the
# matrix, nxt is the next empty row to be used, and inc is the number of
# rows of expansion to be allocated whenever the matrix becomes full

# print sorted tree via in-order traversal
printtree <- function(hdidx,tr) {  
   left <- tr$mat[hdidx,1]
   if (!is.na(left)) printtree(left,tr)  
   print(tr$mat[hdidx,3])  # print root
   right <- tr$mat[hdidx,2]
   if (!is.na(right)) printtree(right,tr)  
}

# initializes a storage matrix, with initial stored value firstval
newtree <- function(firstval,inc) {
   m <- matrix(rep(NA,inc*3),nrow=inc,ncol=3)
   m[1,3] <- firstval
   return(list(mat=m,nxt=2,inc=inc))
}

# inserts newval into the subtree of tr, with the subtree's root being
# at index hdidx; note that return value must be reassigned to tr by the
# caller (including ins() itself, due to recursion)
ins <- function(hdidx,tr,newval) {  
   # which direction will this new node go, left or right?
   dir <- if (newval <= tr$mat[hdidx,3]) 1 else 2
   # if null link in that direction, place the new node here, otherwise
   # recurse
   if (is.na(tr$mat[hdidx,dir])) {  
      newidx <- tr$nxt  # where new node goes
      # check for room to add a new element
      if (tr$nxt == nrow(tr$mat) + 1) {  
         tr$mat <- 
            rbind(tr$mat, matrix(rep(NA,tr$inc*3),nrow=tr$inc,ncol=3))
      }
      # insert new tree node
      tr$mat[newidx,3] <- newval
      # link to the new node
      tr$mat[hdidx,dir] <- newidx
      tr$nxt <- tr$nxt + 1  # ready for next insert
      return(tr)
   } else tr <- ins(tr$mat[hdidx,dir],tr,newval)
}
