## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

## This function is for cache potentially time-consuming computations. 
## makeCacheMatrix is for initialize inverse property


makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the y
  set <- function( y ) {
    x <<- y
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
    ## Return the matrix
    
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() i
    ## Return the inverse property
    
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
