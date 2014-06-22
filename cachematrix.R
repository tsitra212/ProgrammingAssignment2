## cachematrix.R contains 2 functions that create a special object that 
## stores a matrix and cache's its inverse. 

## makeCacheMatrix() is a list of 4 functions that stores a matrix
## and its inverse. The matrix and inverse are stored in cache.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve() function calculates the inverse matrix and returns the inverse. 
## If the inverse is cached, this function returns the cached value.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
