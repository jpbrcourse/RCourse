## Calculate the inverse of a matrix usning a cached version if available

## Create a matrix structure to hold the cached results
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	## Functions are set, get, setinvert, and getinvert
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinvert <- function(solve) m <<- solve
	getinvert <- function() m

	## Create list of the functions
	list(set = set, get = get,
		setinvert = setinvert,
		getinvert = getinvert)
}


## Get the inverse of a matrix - use the cached version if available
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x' (a result of makeCacheMatrix)
	m <- x$getinvert()
	
	## Check if the matrix structure has already been created
	## If so, return the cached results
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## Not already created - get the matrix
	data <- x$get()
	
	## Solve for the inverse
	m <- solve(data, ...)
	
	## Set it into the matrix structure then return
	x$setinvert(m)
	m
}
