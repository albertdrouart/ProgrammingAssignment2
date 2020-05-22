## These two functios work together to return and cache the inverse of a matrix.


## makeCacheMatrix creates an object where the reusable inverse can be stored
## ("cached") and provides a set of utility functions to obtain that data once
## set.   When passed an Matrix the function will store the value in x which is
## the data, and once the identity matrix is cached will store the value in m

setmakeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(slv) m <<- slv
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#cache solve is passed first the CacheMatrix -- that is a matrix which captures
#state of the underlying matrix and it's identity.  First it checks if the
#CacheMatrix # already has a value.   If it does it will return that value,
#otherwise it will solve it then # set the value of the CacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached solve data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
