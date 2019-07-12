
## This R Programming assignment aims to write two functions: "makeCacheMatrix" and "cacheSolve"
## Matrix inversion can be a costly computation and there can be a benefit to caching the inverse of a matrix 
## instead of repeatedly computing it. Below are two functions used to create a special object that stores 
## a matrix and caches its inverse.

## The function "makeVector" creates a special matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The "cacheSolve" function computes the inverse of the special matrix created by the "makeCacheMatrix" above. 
## If the inverse has already been calculated - and the matrix has not changed - then it retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
