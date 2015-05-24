## The functions in this file find the inverse matrix for
## a square, invertible matrix and cache the result,
## potentially saving lots of computation time later.
##
## To use (an example):
##      someInvertibleSquareMatrix <- diag(3)
##      cacheMatrix <- makeCacheMatrix()
##      cacheMatrix$set( someInvertibleSquareMatrix )
##      result <- cacheSolve(cacheObject)
##
## Then run cacheSolve again and notice the faster response:
##      result <- cacheSolve(cacheObject)
##

## Creates a function list to help with caching of a matrix value.
makeCacheMatrix <- function(x = matrix()) {
  # Data of our "object"
  m <- NULL
  
  # Define the methods of our "object"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  
  # Return list of items we want to expose publicly
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Returns the inverse matrix given a "special" function
## list result from 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
  ## Check if the inverse matrix has already been calculated
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  ## No cached data yet, so compute the inverse of 'x', save the result for later,
  ## and return the computed value.
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
