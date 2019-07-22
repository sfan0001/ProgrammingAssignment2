#Purpose: Write a pair of functions that cache the inverse of a matrix


#The function makeCacheMatrix creates a special "matrix" object
#that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function() inverse <<- solve(x)
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse      
}
