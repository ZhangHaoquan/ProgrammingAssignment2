## Assignment 2: Caching the Inverse of a Matrix
## This set of R code will demonstrate how we can cache the 
## result of a Matrix Inversion - a potentially time-consuming computation.

## The function makeCacheMatrix creates an enhance version 
## of the base matrix. In particular, should a matrix inverse
## be called on this new object at least once, it has the 
## ability to remember the result and use it the next time

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatInv <- function(solve) m <<- solve
  getMatInv <- function() m
  list( set = set, get = get, 
        setMatInv = setMatInv,
        getMatInv = getMatInv )

}

## The function cacheSolve works with the object produced 
## by makeCacheMatrix. It taps on the object's memory and
## avoids recomputation of the inverse operation when it
## checks that an existing result exists.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setMatInv(m)
  m
  
}

## --------------------------------------- ##
## To test out the code, try the following ##
## --------------------------------------- ##

## Create an 'Enhanced' matrix 
## y <- makeCacheMatrix(matrix(rnorm(2000*2000),2000,2000))

## Compute the inverse once (Takes some time...)
## z <- cacheSolve(y) 

## Compute it again and see that it reads the cache instead of doing the computation again (Much Faster)
## z <- cacheSolve(y) 
