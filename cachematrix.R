## Computing a matrix inverse can require substantial computing time. The 
## functions here work as a wrapper for the matrix itself as well as a 
## cache container for the computed inverse.

## makeCacheMatrix creates a "matrix" with methods to get and set the data
## and the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setinv <- function(inverse) {
		inv <<- inverse
	}
	getinv <- function() {
		inv
	}
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve first looks to see if there is a cached inverse of the matrix
## and if not it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getted cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
