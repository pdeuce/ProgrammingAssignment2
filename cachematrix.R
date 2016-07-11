## PD comment- this function makeCacheMatrix takes a matrix object and stores it in the cache memory for later comparison.
## Matrix inversion is a time consuming step and there is benefit associated with caching the operation. 

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

## PD comment- This function will compute the matrix inverse from the above  
## makeCacheMatrix function given a matrix table. If the matrix remains the same and we have already  
## calcualted from the function above, it should just retrieve the inverse matrix
## from cahce memory without recalc again. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

##end code for programming assignment 2
