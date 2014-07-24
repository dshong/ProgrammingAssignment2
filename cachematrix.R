## Script includes two functions: makeCacheMatrix and cacheSolve.
## Together, these functions will calculate the inverse of a given matrix,
## or allow results (inverted matrices) from previously calcuated instances 
## of the given matrix to be called.


## makeCacheMatrix: this function returns a list of functions that describe how a matrix 
## and its inverse will be cached or called.

## Function named as makeCacheMatrix with input "x", (where x is a square matrix).
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL

  ## set: caches a given matrix, by allowing a matrix variable to be modified even outside
  ## a particular local environment.
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## get: will return the cached matrix.
  get <- function() x
  
  ## setinverse: caches the inverse matrix (calculated in cacheSolve), by allowing the
  ## inverse variable be modified outside the local environment.
  setinverse <- function(inverse) s <<- inverse
  
  ## getinverse: returns the inverse matrix.
  getinverse <- function() s
  
  ## list command aggregates the previously defined four functions into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: this function calcuates the inverse of a matrix after first searching
## for results of previous calcuations of a given matrix.


## function named as cacheSolve with input "x", (where x is the makeCacheMatrix function
## defined for a specified matrix).

cacheSolve <- function(x, ...) {

  ## function checks makeCacheMatrix for cached inverse of a specificed matrix, 
  ## if located (i.e., not a null value), then the previously calculated inverse 
  ## matrix is returned.
  s <- x$getinverse()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
  
  ## if no cached inverse matrix is located in makeCacheMatrix, the given matrix is 
  ## called and an inverse is calculated using the solve function. The inverse is then
  ## then cached, and returned.
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
