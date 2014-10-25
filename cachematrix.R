## These functions are used together to calculate and lazily cache the inverse of the 
## matrix passed in. The input matrix must be square and invertible, otherwise an error 
## will occur. 

## This function creates the matrix object which contains the cache variable.
## The list returned has the following functions:
##     get() - returns the matrix (non-inverted)
##     set(y) - sets the matrix, and clears the cached inverse matrix
##     getInverse() - returns the cached inverse matrix (will never calculate it)
##     setInverse(inv) - sets the inverse matrix, i.e. the cache value.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverseMatrix <<- inv
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## get the inverse of the matrix, returning the cached value if it has already
## been calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  message("calculating")
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}



