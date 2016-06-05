## Creates a "matrix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  message("cached inverse not available, so going to calculate it")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
}

