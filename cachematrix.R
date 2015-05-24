## The following 2 functions take the inverse of a matrix
## unless the inverse has already been taken in which case
## the inverse is retrieved from the cache


## makeCacheMatrix creates a list of functions that set
## the matrix, get the matrix, set the inverse, and get 
## the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the inverse of the matrix x
## if the inverse has already been taken, then 
## the inverted matrix is retrieved from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}
