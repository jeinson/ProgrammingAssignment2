## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions that are used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL # declare variable to hold inverse
	# assigns the input matrix as a varible in the higher environment
	set <- function(y) { 
		x <<- y 
		i <<- NULL
	}
	
	# returns the original matrix
	get <- function() x 
	
	# sets the calculated inverse to the variable "i" in the higher environment
	setinverse <- function(inverse) i <<- inverse 
	
	# returns the calculated inverse
	getinverse <- function() i
	
	# a list of functions to be used by cacheSolve
	list(set = set, get = get,
		setinverse = setinverse, 
		getinverse = getinverse)

}


## This function either returns the cached value of the inverse of x, 
## or calculates it itself and returns it

cacheSolve <- function(x, ...) {
        # assigns the inverse of x to i, if it exists and has been calculated
	i <- x$getinverse()
	
	# conditional. if the inverse exists in the cache, it is returned along with a message
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	# otherwise, the matrix is assigned to "data"
	data <- x$get()
	
	# the inverse of data is calculated and set to "i"
	i <- solve(data, ...)
	
	# the inverse of the original matrix is saved in the cache
	x$setinverse(i)
	
	#returns i
	i
}
