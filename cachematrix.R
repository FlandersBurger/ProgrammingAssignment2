## This file contains functions to contruct an inverted matrix and store it

## Below function will constructs the matrix functions and attributes
## The matrix needs to have equal dimensions

makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- NULL
	set <- function(y) {
		x <<- y
		inverse_x <<- NULL
	}
	get <- function() x
	setinverse <- function(x) inverse_x <<- x
	getinverse <- function() inverse_x 
      list(set = set, get = get,
      	setinverse = setinverse,
      	getinverse = getinverse)
}


## Below function will return the cached inverse matrix if one is found
## If no cache is found it will create the inversed matrix 

cacheSolve <- function(x, ...) {
	inverse_x <- x$getinverse()
	if(!is.null(inverse_x)) {
		message("getting cached data")
		return(inverse_x)
	}
	data <- x$get()
	inverse_x <- solve(x)
	x$setinverse(inverse_x)
	inverse_x
}
