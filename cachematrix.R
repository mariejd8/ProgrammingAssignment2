## Here we are caching the inverse of a Matrix

## This function creates a matrix object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolveMat <- function(solve) m <<- solve
  getsolveMat <- function() m
  list(set = set, get = get,
       setsolveMat = setsolveMat,
       getsolveMat = getsolveMat)
}

## This function either retrieves the cached inverse matrix solution
## or solves new if the matrix is changed.
cacheSolve <- function(x, ...) {
  m <- x$getsolveMat()
  if(!is.null(m)) {
    message("getting cached solved matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolveMat(m)
  m
}