## Helper functions to create an object that contains a matrix and its cached inverse.

## Constructor function for making a "cached" matrix "object".
## The object is a list containing methods to get and set the underlying matrix.
## The object also contains methods to get and set the matrix inverse.
## This function takes advantage of R's closure's capabilities to store the matrix and its data.
## Note how all the functions "close" over the inverse and x variables.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    # Clear the inverse when a new matrix is set.
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to get the cached inverse of a matrix.
## If the inverse hasn't been previouly computed, the function solves the matrix and store the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  matrix_data <- x$get()
  inv <- solve(matrix_data)
  x$setInverse(inv)
  inv
}
