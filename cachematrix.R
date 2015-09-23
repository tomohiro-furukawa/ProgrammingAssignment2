## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a square matrix as a parameter and returns a cached-matrix object that caches the calculated inverse matrix of itself.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function( i ) inv <<- i
  getinv <- function() inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## cacheSolve takes a cached-matrix object and returns an inverse matrix of the original matrix. If cached-matrix has already calculated inverse matrix, this function returns it immediately.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if( !is.null( inv ) ) {
    message( "getting cached inverse" )
    return( inv )
  }
  mat <- x$get()
  inv <- solve( mat, ... )
  x$setinv( inv )
  inv
}
