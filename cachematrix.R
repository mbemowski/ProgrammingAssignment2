## A pair of functions that allows to create special matrix object that can
## cache its inverse and calculate the inverse or get the cached value.

## Creates a matrix object that can cache its inverse.
## Argument x is a matrix which will be represented by this object.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns an inverse of a matrix object created by the makeCacheMatrix 
## function. Argument x is the matrix object to be inverted.
## If the inverse of the matrix has already been calculated then
## the inverted matrix is obtained from the cache. Otherwise it is
## calculated and stored in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(is.null(inv)) {
    message("calculating matrix inverse")
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
  }else{
    message("getting cached data")
  }
  inv
}
