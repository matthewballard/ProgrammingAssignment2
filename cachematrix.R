## These functions create a special matrix object that can cache its inverse in memory
## and pull it back instead of computing it every time it is needed

## makeCacheMatrix creates a "matrix" object that holds a matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Create the "matrix" object, and set the inverse to null
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## Function to return the "matrix"
  get <- function() {x}
  ## Function to cache the inverse
  setinverse <- function(inverse) {i <<- inverse}
  ## Function to return the cached inverse
  getinverse <- function() {i}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve either finds the inverse of the  matrix, or retreives the previously cached inverse from memory

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  ## Return a cached matrix that is the inverse of 'x', if it exists
  if(!is.null(i)) {
    message("getting cached value")
    return(i)
  }
  ## Otherwise, solve for the inverse of 'x', and cache it
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
