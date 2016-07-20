## creating a special "matrix" object that can cache its inverse.

## functions below are used to create an object where there
## the matrix is stored and its inverse is cached.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The inverse of the special matrix created by the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if (!is.null(inv)){
              message("getting cached data")
              return(inv)
      }
      m <- x$get()
      inv <- solve(m, ...)
      x$setInverse(inv)
      inv
}
