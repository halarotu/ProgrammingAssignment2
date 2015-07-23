## makeCacheMatrix and cacheSolve are functions to handle matrix data

## The first function, makeCacheMatrix creates a
## list containing functions to
## set and get the value of the matrix, and
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function, cacheSolve returns a matrix that is the inverse of 'x'.
## If the inverse matrix has been solved before, the old value is returned.
## Else, the inverse matrix is to be solved and returned.

cacheSolve <- function(x, ...) {
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
