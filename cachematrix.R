## These functions help to implement caching when calculating an inverse of a matrix.
## If the value has already been calculated, then the cache version is used; else
## the inverse is calculated, then cached for later use.

## The function to use when constructing a matrix with a cache.  This should
## be used instead of calling the matrix function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function does the work of checking to see if the inverse has already been calculated
## and handling the case where it has not.  The input value x must be a makeCacheMatrix class
## for this function to work

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
}


