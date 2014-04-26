## The below functions are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "list", which sets and gets
## the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                   
    x <<- y                       #set the value of the matrix  
    m <<- NULL                    # NULL = inverse not calculated yet
  }
  get <- function() x             #get the stored value of matrix
  setInverse <- function(solve) m <<- solve     # store calculated inverse
  getInverse <- function() m                    # retrieve stored inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the matrix created with
## the above function. It checks to see if the inverse has been calculated.
## If so, it retrieves the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and stores the value in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()                 # retrieve cached inverse
  if(!is.null(m)) {                   # if inverse has been calculated previously
    message("getting cached data")    
    return(m)
  }
  data <- x$get()                   
  m <- solve(data, ...)               # calculates the inverse of the matrix
  x$setInverse(m)                     # sets the value of inverse in the cache
  m                                   # Return a matrix that is the inverse of 'x'
}
