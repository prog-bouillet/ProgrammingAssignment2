## 
## This file implements a cached matrix inversion.
## Call makeCacheMatrix to create the cached matrix object
## Call cacheSolve to solve for the matrix inverse and initialize the cached result
## Subsequent calls to cacheSolve will return the precomputed result unless the 
##   data has changed
##

## makeCacheMatrix implements the persistent cache interface of the cached 
## matrix object. The following function is the inversion solver that uses
## cached data if it is available (matrix inversion has already been solved once).
## cacheSolve could be integrated into makeCacheMatrix rather than being a separate
## function, but this is how the assignment is asked to be structured.

makeCacheMatrix <- function(x = matrix()) {
   
   m <- NULL

   setdata <- function(y) {
      x <<- y
      m <<- NULL
   }
   
   getdata <- function() x
   
   setinv <- function(xinv) m <<- xinv
   getinv <- function() m
   
   list(setdata = setdata,
        getdata = getdata,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve returns the precomputed result if cached data is available, and
## otherwise computes the matrix inverse first and loads the reult into the cache

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getinv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$getdata()
   m <- solve(data, ...)
   x$setinv(m)
   m
}
