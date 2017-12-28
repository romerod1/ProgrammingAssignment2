## The following functions allow to cache the inverse of a matrix

## Makecachematrix produce a list  containing a function to: set the matrix, get the 
##matrix, set the inverse and get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  setmatrix <- function(y) {
    x <<- y
    s <<- NULL
  }
  getmatrix <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Cachesolve requires an argument that is returned by makeCacheMatrix() 
##in order to retrieve the inverse from the cached value that is stored 
##in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$getmatrix()
  s <- solve(data,...)
  x$setsolve(s)
  s
  ## Return a matrix that is the inverse of 'x'
}