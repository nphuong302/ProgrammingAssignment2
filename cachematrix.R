##  The function generates a cache for a matrix that may be used to store the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve() function computes the inverse of a matrix and caches the result in a cache built by the makeCacheMatrix() function. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  if (det(matrix_to_invert) == 0) {
    warning("Matrix is singular and cannot be inverted.")
  } else {
    inv <- solve(matrix_to_invert, ...)
  }
  x$setinverse(inv)
  inv
}