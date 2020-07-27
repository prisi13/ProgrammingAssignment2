## Put comments here that give an overall description of what your
## functions do
##Aim is to write a pair of functions that cache the inverse of a matrix.
##So two functions named makeCacheMatrix and Cachesolve are written.
## Write a short comment describing this function
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

