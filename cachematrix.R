## Correct code functionality with comments

## First function constructs a list of 4 functions which are used to generate the 
## inverse of a supplied matrix using the solve() function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Second function evaluates if there is an existing cached value for the inverse
## of a matrix. If so, this is displayed. If not, the inverse operation is carried out on
## the supplied matrix in the function call (x).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If the variable 'm' is not empty, the cached value is returned.
  if(!is.null(m)){
    message("Previous result exists. Retrieving.")
    return(m)
  }
  ## If 'm' is empty, then the inverse is calculated.
  matrixdata <- x$get()
  m <- solve(matrixdata, ...)
  x$setinverse(m)
  m
}