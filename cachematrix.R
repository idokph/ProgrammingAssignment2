## This pair of functions will cache the inverse of a matrix and return the calculation from chace if its already calculated, 
## otherwise it will compute the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
  )
  
}

## this function will calculate the inverse of the matrix set.
## if its already chaced, it will return the cached matrix
## otherwise it will be calculated

CacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  inv
  
}

## Return a matrix that is the inverse of 'x'