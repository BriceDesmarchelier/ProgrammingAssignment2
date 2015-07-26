## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix" that sets, gets both the matrix and its inverse, and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}



## Calculates the Inverse of a Matrix only if it has not been calculated and cached before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)  ## replaces the value in the list by the makeCashMatrix
  m
}
