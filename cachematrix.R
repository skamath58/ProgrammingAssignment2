## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and supporting functions to
## get the matrix, set the inverse abd get the inverse return the
## list of the methods supported on the matrix. No check is made
## for the existence of an inverse, it is assumed to exist.

## i is the inverse of the x matrix, gets set in the setinverse function

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}



## This fucntion returns the inverse of a matrix when passed
## an object that contains the matrix created using the makeCacheMatrix,
## a function previously described.

## When requested, the cache is checked to see if the value is present
## If found, then it id returned, else the inverse is computed using solve()
## then the inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}
