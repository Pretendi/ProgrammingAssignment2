## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function is extremely similar to the makeVector from the example, and contains that same functionality but for matrices (set and get matrix, set and get inverse)

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL

	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() x

	setinv <- function(inverse2) inverse <<- inverse2
	getinv <- function() inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

# Returns a matrices inverse, either by getting the cached data or by calculating it again (where required).

cacheSolve <- function(x, ...) {
       
	inverse <- x$getinv()

	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	matrix_input <- x$get()
	inverse <- solve(matrix_input)
	x$setinv(inverse)
	inverse
}
