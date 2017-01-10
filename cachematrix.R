## This file contains 2 fuctions, one to create a cache matrix, the other to create solved
## inverse matrices

## makeCacheMatrix contains a list of 4 list values that contain getter/setter functions

makeCacheMatrix <- function(x = matrix()) {
  
  #get matrix
  #set matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  #set inverse
  #get inverse
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve checks to see if a pre-cached version exists for a matrix

cacheSolve <- function(x, ...) {
      
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
      ## Return a matrix that is the inverse of 'x'
      ## search the cache
      ## if found, return that
      ## if not, cache and return the obect
}
