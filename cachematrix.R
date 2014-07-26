## makeCacheMatrix: caches a matrix's inverse
## cacheSolve:  solves the inverse of matrix but checks if there's a cached
##              and if not saves a cached version of the result

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # intialise the inverse
  set <- function(y) { # function 'set' assigns the vector to m
    x <<- y
    m <<- NULL
  }
  get <- function() x # this retrieves the matrix
  setinverse <- function(solve) m <<- inverse # this stores the inverse
  getinverse <- function() m # this retrieves the cached inverse
  list(set = set, get = get, # returns a list which can be accessed like methods via $
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() # checks for inverse and assigns it to m
  if(!is.null(m)) { # if m is not empty, then retrieve the cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # otherwise, solve the matrix inversion
  x$setinverse(m) # then also save the inverse into the cache
  m
}
