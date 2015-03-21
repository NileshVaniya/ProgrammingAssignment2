## The following function will create a special kind of 'Matrix' (a list) which returns
## four functions - get, set, getInverse and setInverse. The aim for this functions is to
## get/set values from/in cache.

## makeCacheMatrix creates a special 'Matrix'

makeCacheMatrix <- function(x = matrix()) {
  inv <- null
  set <- function(y,...) {
    x <<- y
    inv <<- null
  }
  get <- function() x
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() inv
  list (set=set, get=get, setInverse = setInverse, getInverse=getInverse)

}

## cacheSolve checks the cache, it will return inverse from cache if already calculated
## else will calculate and return the inverse. 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(x_inv)) {
          print("getting inverse from cache...")
          return (inv)
        }
        data <- x$get()
        inv <- solve(x,...)
        x$setInverse(inv)
        inv
}
