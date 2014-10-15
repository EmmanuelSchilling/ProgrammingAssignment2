## The functions in this file can be used to represent a square matrix which 
## is capable of caching the value of its inverse matrix.  
##
## The makeCacheMatrix() function will create a special "matrix" object that
## can cache its inverse.
##
## The accompanying cacheSolve() function will operate upon this "special" matrix.
## cacheSolve() will return the cached value of the inverse matrix, if it has
## been computed previously and the underlying matrix is unchanged.  If the cached
## inverse value is non-existent or obsolete the new inverse value is calculated
## and saved for future reference.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
