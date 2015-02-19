## Matrix inversion is a costly computation, so it's better to cache the result
## of the inverse of matrix rather than computing it repetedly.   

## makeCacheMatrix is a function which creates a list containing functions to
#  set the value of function.
#  get the value of a function.
#  set the value of the inverse of the matrix.
#  get the value of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<-NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv ) 

}


## cacheSolve
# This function returns the inverse of a matrix. It will first check if the 
# inverse is already computed. If so,  it gets the inverse from the cache 
# and skips computation. Otherwise it computes the inverse of the given matrix
# and sets the value of the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
