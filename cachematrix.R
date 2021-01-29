## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix is a function that creates a special 'matrix' object
## that can cache its inverse
## matrix value can be retrieved with get function
## cached value of matrix can be changed with set function
## cached inverse of the matrix can be retrieved with getinv function
## setinv function to get access to the object from cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}

## Write a short comment describing this function
##
## cacheSolve is a function that computes the inverse of the special 'matrix'
## returned by the makeCacheMatrix function. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.
## cacheSolve gets access to the object's inverse matrix with the "<-"
## operator that searches in the function's environment first and then 
## searches in the parent environment, returning the cached inverse matrix
## if it is found, otherwise it computes the inverse and returns it

cacheSolve <- function(x,...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
