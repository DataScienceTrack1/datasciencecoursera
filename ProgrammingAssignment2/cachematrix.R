## This assignment involves putting comments here that give an overall
## description of what the functions do.

## Here is a pair of functions that cache the inverse of a matrix.
## It creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
  ## First, initialize the inverse property
  i <- NULL
  ## This is the method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## This is the method the get the matrix
  get <- function() {
    ## This returns the matrix
    m
  }
  ## This is the method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## This is the method to get the inverse of the matrix
  getInverse <- function() {
    ## Then, return the inverse property
    i
  }
  ## Then, return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## First, we compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then "cacheSolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## First, return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Next, just return the inverse if it is already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Then, get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  ## And then set the inverse to the object
  x$setInverse(m)
  ## Then, return the matrix
  m
}