## Put comments here that give an overall description of what your
## functions do

## This function builds a data structure (namely a list)
## which contains the matrix given as input and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  
  ## inv will contain the inverse of the matrix x
  inv <- NULL
  
  ## set the new matrix and clean the value of the inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the original matrix
  get <- function() {
    return(x)
  }
  
  ## set the value of the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    return(inv)
  }
  
  ## return the data structure
  return(list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}


## This function computes the inverse of a (square) matrix
## and makes use of the cache in the data structure
## to avoid re-computing the inverse matrix every time
## the cacheSolve function is called.
cacheSolve <- function(x, ...) {
  
  ## Get the inverse of the matrix
  inv <- x$getInverse()
  
  ## If there is already a cached inverse matrix, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Otherwise compute inverse matrix and store it
  ## Get the original matrix
  data <- x$get()
  
  ## Compute the inverse
  inv <- solve(data, ...)
  
  ## Store the inverse in the data structure
  x$setInverse(inv)
  
  ## Return the inverse matrix
  return(inv)
        
}
