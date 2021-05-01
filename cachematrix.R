## The pair of functions take the input matrix, evaluate its inverse and cache it. In case the inverse is already cached, it retrieves the same


## This function takes as an input, a matrix x and calls another function to evaluate its inverse. its inverse is then cached

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This evaluated the inverse of the matrix.If it has already been cached,it retrieves it

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}
