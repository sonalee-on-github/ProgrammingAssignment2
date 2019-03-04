## The following functions deal with matrix operations in R, allowing users to perform functions
## that are not already built into existing functions.

## The first function, makeCacheMatrix, creates a "matrix" object that can cache the value of its inverse
## or future reference and use by other functions

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


## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve
## the inverse from the cache.

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
    
  ## Return a matrix that is the inverse of 'x'
}
