## These two functions work together to optimize the inverting of a matrix.  
## The first time the cacheSolve routine is called, the inverted matrix is 
## cached, so it won't need to be recalculated if it's needed again.

## To call these functions, use the following sample code:
##  x<-matrix(c(1:4), ncol=2, nrow=2)
##  y<-makeCacheMatrix(x)
##  cacheSolve(y)

## This function contains several small functions.  They are:
## get() - returns the matrix to be solved
## set() - saves the matrix to be operated on into x, and initializes m (the solved matrix)
## getInverse() - returns the solved matrix
## setInverse() - saves the solved matrix into m

makeCacheMatrix <- function(originalMatrix = matrix()) {
  solvedMatrix <- NULL
  set <- function(y) {
    originalMatrix<<- y
    solvedMatrix<<- NULL
  }
  
  get <-function() originalMatrix
  setInverse <- function(solve) solvedMatrix <<- solve
  getInverse <- function() solvedMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This routine first attempts to retrieve the value of getInverse from the
## cached matrix.  If that value is not null, it returns it.  If it is null, 
## the routine must first calculate the inverse, and then it saves it using 
## the setInverse function.  The value is then returned.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  message("calculating inverse")
  data <- x$get()
  m<-solve(data, ...)
  x$setInverse(m)
  m
}
