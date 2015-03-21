## The defined functions are:
## - makeCacheMatrix: creates the cache function vector
## - cacheSolve: determines the inverse of a matrix

## Usage e.g.
##   fill an invertible matrix
##     v=c(1,2,3,4,1,6,7,8,1)
##     m <- matrix(v,nrow=3,ncol=3)
##   put matrix in the cache
##     mc <- makeCacheMatrix(m)
##   Use it from the cache
##     cacheSolve(mc)

## This function creates a special "matrix" object thean can
## cache its reverse: the solve() function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL   
  # define the four cache functions: set, get, setsolve, getsolve
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # extracts the matrix data
  get <- function() x 
  # initialize setsolve
  setsolve <- function(solve) m <<- solve
  # initialize getsolve to NULL
  getsolve <- function() m
  # add functions to the cachelist  
  list( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function determines the inverse of a matrix. If it is allready
## calculated it will extract the result from a cache, if not it will 
## calculate the inverse and put it in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if (!is.null(m)) {
    # the result is cached
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # calculate the inverse of the matrix
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
