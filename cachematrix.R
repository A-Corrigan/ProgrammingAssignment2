## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve calculates the mean of the matrix created with makeCacheMatrix.

## Write a short comment describing this function
## Parent function takes in the argument matrix which we assume is invertible
## The value of the matrix is set using the child function
## The double arrow operator is used to assign a value to x and inv in an environment that is different from the parent environment
## Returns the value of the matrix and sets and returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x<<- y
		inv<<- NULL
	}
	get <- function() {x}
	setInverse <- function(inverse) {inv <<- inverse}
	getInverse <- function() {inv}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Returns a matrix that is the inverse of X, assigning it to inv
## Also checks if the inverse has already been calculated, if so it gets it from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
	  if(!is.null(inv)){
		message("retrieving cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
