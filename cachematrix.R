## The makeCacheMatrix defines a list of functions for setting and
## getting the matrix and its inverse, along with the matrix and 
## inverse variables themselves.

## The cacheSolve checks the given function list to see if the inverse
## is already in cache. It reads the cache if the latter is present, 
## and otherwise compute the inverse, put it in the cache and return
## its value.

## Construct the four functions

makeCacheMatrix <- function(x = matrix()) {
  invMat = NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  getInv <- function() invMat
  setInv <- function(mat) invMat <<- mat
  list(set = set, get = get, 
       getInv = getInv, setInv = setInv)
}


## Check cache, use cache if available, otherwise compute the inverse
## of the matrix, store it in cache and return it.

cacheSolve <- function(x, ...) {
        invMat <- x$getInv()
        if(!is.null(invMat)) 
          return(invMat)
        mat <- x$get()
        if(!is.matrix(mat) || is.na(sum(mat))) {
          message("Matrix not set yet")
          return()
        }
        invMat <- solve(mat)
        x$setInv(invMat)
        return(invMat)
}
