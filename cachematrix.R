
## this function creates a list of functions for set, get, getinverse and setinverse
##the returned list needs to be stored in a variable to be called in the cacheSolve function

makeCacheMatrix<- function(x = matrix()) {
  i <- NULL ## this creates a null value for i
  set <- function(y) {    ## this function sets the value for x and i
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ## then a list is created containing the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## this function solves the matrix, if the matrix has been solved
## before, then it will return the cached value
cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) ## this occurs if the matrix has been solved already
  }
  data <- x$get()
  i <- solve(data, ...)  ## this is where the matrix is solved
  x$setinverse(i)
  i
}

