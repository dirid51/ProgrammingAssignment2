## Created by Randall Booth on October 25, 2015
## for the Coursera course "R Programming".

## These functions work together to enable the caching
## of the inverse of a matrix.


## Creates a matrix that is used to store, or cache, the inverse
## of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function()
    x
  setinverse <- function(solve)
    matrixInverse <<- solve
  getinverse <- function()
    matrixInverse
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Determines the inverse of a given matrix. If the
## matrix inverse has already been calculated, then
## the value is retrieved from the cache.

cacheSolve <- function(cache, ...) {
  matrixInverse <- cache$getinverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- cache$get()
  matrixInverse <- solve(data, ...)
  cache$setinverse(matrixInverse)
  matrixInverse
}
