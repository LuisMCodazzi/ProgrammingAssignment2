## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  # makeCacheMatrix <-function(m=matrix()) {
  inv <- NULL
  set <- function(y) {x <<- y ; inv <<- NULL}
  get <- function() x
  setinv <-  function(y) inv <<- y
  getinv <-  function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # cacheSolve <- function(m,...) {
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

