## The functions "makeCacheMatrix" and "cacheSolve" combinedly makes 
## it possible to cache the Inverse of a Matrix in memory so that it is not
## required to compute the value every time it is used.
## The value of the matrix inverse is set in the "inv" variable which is 
## part of the container function "makeCacheMatrix".
## When the setInverse() function is invoked, it actually sets this value
## to the container variable so that when the getInverse() function is invoked
## after this the "inv" value is already available in the cache.

## The function "makeCacheMatrix" takes a Matrix as an input
## and returns a list. This list is actually a list of functions
## for getting and setting the values of the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	getInverse <- function() inv
	setInverse <- function(m) inv <<- m
	get <- function() x
	set <- function(y) {
		inv <<- NULL
		x <<- y
	}
	list(set = set, get = get,
		 getInverse = getInverse,
		 setInverse = setInverse)
}


## The function "cacheSolve" fetches the inverse of a matrix from cache by calling
## the getInverse() function. If it gets a non NULL value then the inverse
## is returned as output. Else if the value of the inverse is NULL then it
## computes the inverse, sets the value to the cache and returns it as ouput.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if(!is.null(inverse))
	{
		message("Getting cache data")
		return(inverse)
	}
	inverse <- solve(x$get())
	x$setInverse(inverse)
	return(inverse)
}
