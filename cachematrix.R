## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and later on offers
## the special functionality of a cached solve method, i.e.
## it allows to store the result of the solve method (the inverse
## matrix) in cache and therefore improving the run time.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the given matrix.
## Note: we assume that the given matrix always has an inverse matrix.
## If the inverse matrix has not been calculated before, it is calculated,
## otherwise the inverse matrix stored in the cache is returned without
## needing to calculate it again.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  i
}