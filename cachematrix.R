## Put comments here that give an overall description of what your
## functions do
##Aim is to write a pair of functions that cache the inverse of a matrix.
##So two functions named makeCacheMatrix and Cachesolve are written.
## Write a short comment describing this function
##This function is used to create a special matrix object that can cache its inverse
## I simply set the input x as a matrix
## and then set the solved value "j" as a null

makeCacheMatrix <- function(x = matrix()) {
 
   j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
 }get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

