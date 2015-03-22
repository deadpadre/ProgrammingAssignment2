## Sometimes computation of the inverse of the given matrix may be a way too expensive operation.
## So we'll cache (e.g. temporarily save) the inverse and wait for further matrix changes. 
## In case of changes we will forget the cached value and count the inverse from scratch.

## Creates a list containing setters and getters both for the passed matrix and the variable 
## treated as its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## `inv` is NULL in the beginning and if there were some changes affected the matrix.
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


## Basically it returns a matrix that is the inverse of 'x', but it only actually computes it,
## when there were some matrix changes, otherwise it just throws out cached value.
## It also assumes that passed matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ## If there were no changes, return cached value.
  if (!is.null(inv)) {
    message("retrieving cached value")
    return(inv)
  }
  
  ## `solve` is R-predefined function which computes the inverse of the given matrix.
  inv <- solve(x$get())
  x$setInverse(inv)
  inv
}