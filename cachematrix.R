## The functions create a cache of inverse of a matrix and make
## use of it if available

## This function provides a vector to set and get cache of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix) inverseMatrix <<- matrix
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets input as vector from the above function and returns the inverse
## It gets the inverse from cache or computes inverse of a matrix and set it in cache (if inverse not available in the cache)

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
