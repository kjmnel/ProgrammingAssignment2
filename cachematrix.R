## The purpose of these functions is to make the
## computation of inverting matrix quick by getting
## a special matrix back that can cache its inverse 

## Creates a special matrix that can be cached

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  ##passes make a list that makes the functions accessible
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Uses an object returned by makeCacheMatrix to return a
##the inverse of the matrix. upon the second call of the 
## function it will return a cached copy
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
    
      ## Return a matrix that is the inverse of 'x'
}
