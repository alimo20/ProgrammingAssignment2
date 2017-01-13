## This file contains 2 functions, one to create a cache matrix, the other to solve the matrics

## makeCacheMatrix contains a list of 4 functions that are the getter/setter functions

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  #set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get matrix
  get <- function() x
  
  #set inverse
  setinverse <- function(solve) m <<- solve
  #get inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve checks to see if a pre-cached version exists for a matrix

cacheSolve <- function(x, ...) {
    
  ## search the cache
  m <- x$getinverse()
  ## if found, return that and exit the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if not, cache and return the obect
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
