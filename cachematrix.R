## Put comments here that give an overall description of what your
## functions do

## Creates a custom object (a list) to store the matrix and cached inverse
## if setInverse had been invoked. Cache will be cleared if matrix value is set
## to another value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Use underlying 'solve' function to find the inverse of the matrix.
## It will retrieve cached result if the function had been called with the same
## cache matrix object before.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    ## return cached
    ## print("Return cached result")
    return (m)
  }
  m <- solve(x$get(), ...)
  x$setInverse(m)
  m
}
