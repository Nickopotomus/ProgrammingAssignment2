## makeCacheMatrix creates a "family" of matrix functions specific to matrix x
## that are then used by cacheSolve to return x-inverse from cached memory (if present)
## or via solve().


## makeCacheMatrix receives matrix variable x and creates a list with function
## members set, get, setInverse, & getInverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve recieves makeCacheMatrix variable x, checks to see if inverse
## already exists in memory.  Returns inverse if it exists; computes inverse
## otherwise
##
## Assumption: Passed matrix is square and inverse exists

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
