## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# return a list of functions to set or get the value of a matrix 
# and set or get the inverse of it.
makeCacheMatrix <- function(x = matrix()) {
    # m_i is the cached inversed matrix, init as null
    m_i <- NULL
    # set the value of the matrix
	set <- function(y) {
        x <<- y
		m_i <<- NULL
	}
	get <- function() x
	# setinverse just keep the matrix inv as the inversed, no checking
	setinverse <- function(inv) m_i <<- inv
	#return the variable kept as the inversed matrix
	getinverse <- function() m_i
	# create the list to be returned
	list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## Write a short comment describing this function
#
# Calculate the inverse of the matrix created with the 
# makeCacheMatrix function, but it first checks to see if it has
# already been calculated and cached, otherwise calculate it using
# solve and put it in the cache via setinverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

	m_i <- x$getinverse()
	# in m_i is not null then it was already calculated and 
	# stored, so it is the cached data
	if(!is.null(m_i)) {
    	message("getting cached data")
		return(m_i)
	}
	data <- x$get()
	# call solve with further arguments to get the inverse...
	# and assume that the matrix is always invertible
	m_i <- solve(data, ...)
	# and cache it
	x$setinverse(m_i)
	m_i

}
