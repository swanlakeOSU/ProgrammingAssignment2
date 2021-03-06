## It includes two functions. The makeCacheMatrix function creates a "matrix" object 
## that can cache its inverse.

## The cacheSolve function computes the inverse of the "matrix" returned by 
## the makeCacheMatrix function above.


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y 
    inver <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inver <<- inverse}
  getInverse <- function() {inver}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setInverse(inver)
  inver
}