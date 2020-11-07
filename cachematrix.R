## Put comments here that give an overall description of what your
## functions do

## This is a function that returns a list
## with functions to set and get a matrix
## and to get and set the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function handles the caching of the inverse of a given matrix
## It checks first if the inverse value of the matrix already exists
## using the makeCacheMatrix$getInverse function, if it already exists
## we can use that, but if it does not we will set the value to cache it
## using the makeCacheMatrix$setInverse

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}
