## Coursera - R PRogramming Week 3 | Programming Assignment 2
## Student: Deon Engelbrecht

## The function makeCacheMatrix gets passed a matrix as input parameter 
## and then stores the matrix and the solve value. Function makeCacheMatrix 
## returns a list with accessor fuctions. The function cacheSolve receive 
## the object returned from function makeCacheMatrix as input parameter and 
## return the invers of the matrix set in function makeCacheMatrix.

## Function makeCacheMatrix takes matrix as input, stores the solve value and returns a list with accessor functions 

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Function cacheSolve takes list from makeCacheMatrix as input and returns the solve value sored.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
