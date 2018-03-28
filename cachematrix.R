## These pair of R functions are able to cache the potentially 
## time-consuming computation of the inverse of a matrix. 
## They are useful in situations where this computation 
## must be done repeatedly

## This function creates a special 'matrix-like' object, 
## capable to cache its inverse matrix. 
## The 'matrix' created can store its Inverse matrix
## in order to avoid repeated calculations

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {x <<- y ; inv <<- NULL}
  get <- function() x
  setinv <-  function(y) inv <<- y
  getinv <-  function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}

## This function computes the inverse of a special "matrix", 
## returned by the makeCacheMatrix function.
## If the matrix has not changed and the inverse has already
## been calculated, then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    mi <- x$getinv()
    if (!is.null(mi)) {
      message("Using cached data ");
      return(mi)
    }
    ma <- x$get()
    mi <- solve(ma,...)
    x$setinv(mi)
    mi
}

