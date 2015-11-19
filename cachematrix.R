## Put comments here that give an overall description of what your
## functions do
##
## These functions allow you to store a matrix in a special way so
##    that the inverse can be cached for quick repeated retrivial 
##
## Sample usage:
## > m <- makeCacheMatrix()
## > m$set( matrix( c(1,2,3,4), nrow=2, ncol=2))
## > cacheSolve(m)
## > cacheSolve(m)
##
## That first call to cacheSolve will perform the work of calculating
##    the inverse. The second call will not need to due to caching. 

 

## makeCacheMatrix allows you to store a matrix in a way that can 
##   cache the inverse for quick retrival in the future. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)

}


## cacheSolve will compute an inverse for a matrix created with makeCacheMatrix.
##   if its been called before the inverse will be cached and will be returned
##   without recalculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
	
}
