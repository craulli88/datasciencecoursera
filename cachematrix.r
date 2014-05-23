## With these two functions a cache will be created for the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  ## Initial value of m is NULL
  set <- function(y) {
    x <<- y  ## set y equal to x at the global level
    m <<- NULL  
  }
  get <- function() x ## Function to get the value of the matrix
  setInverse <- function(inverse) m <<- inverse  ## Set value of the inverse
  getInverse <- function() m  ## Get the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  m <- x$getInverse()
  
  ## This if-statement checks to see if there is already something in 'm'
  ## meaning that is has already been cached, and will return the cached values
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() ## Sets data to the get() finction of makeCacheMatrix
  m <- solve(data, ...) ## Sets m to the inverse of 'data'
  x$setInverse(m)
  m ## print the values of m
}
