# R-Programming Programming Assignment #2 
## October 26th 2014 - cachematrix.R

## This pair of R functions (makeCacheMatrix & cacheSolve) caches the inverse of a given square invertable matrix.
## example of an inputa matrix : amatrix = [1][3] 
##                                         [2][4]

## makeCacheMatrix: this function creates a special matrix object that can cache its corresponding inverse matrix object.
makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialization of inverse property
  inv <- NULL
  
  ## Method - Setting up the matrix object
  set <-  function(matrix){
    inv <<- NULL
    m  <<-  matrix
  }
  
  ## Method - Getting the matrix object
  get <- function() {
    ## Return the matrix object inv
    m
  }
  
  ## Method - Setting the inverse matrix object
  setInverse <- function(inverse.matrix) {
    inv <<- inverse.matrix
  }
  
  ## Method - Getting the inverse matrix object
  getInverse <- function() {
    inv
  }
  
  ## List of methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: this functions compute the inverse of the special matrix object returned by the makeCacheMatrix function above.
## If the inverse was previously computed (moreover, the matrix has remained the same), then cacheSolve shall retrieve inverse matrix object from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return the matrix object representing the inverse of x
  m <- x$getInverse()
  
  # Print the return matrix object if its already set in the cache.
  if(  !is.null(m) ){
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from makeCacheMatrix special object.
  data <- x$get()
  
  ## Compute the inverse matrix 
  m <- solve(data,...)
  
  ## Set the inverse of the object makeCacheMatrix
  x$setInverse(m)
  
  ## Return the matrix
  m
  
}

# Testing & Validation - additional info
## https://class.coursera.org/rprog-008/forum/thread?thread_id=174
