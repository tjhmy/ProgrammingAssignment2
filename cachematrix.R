## In this file, there is code that allows the use of the <<- operator in R in order to allow the assignment of the value of an inverse matrix to an environment different than that within the function itself so that it may be accessed via other functions.  Here, I will write 2 functions in order to create an object that stores a matrix and cache its inverse.

## This function creates a list which contains the functions that set and get the values of the matrix, x, and its inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){									# This function sets the elements of the matrix
		x <<- y
		inv <<- NULL
	}
	get <- function() x									# This function gets the matrix
	setinverse <- function(inverse) inv <<- inverse		# This function sets the inverse value of hthe matix and commits it to the cache
	getinverse <- function() inv						# This function returns the inverse of the matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
}


## This function computes the inverse of the matrix created by the makeCacheMatrix function. It first checks to see whether the inverse has already been calculated and stored in the cache. If it has, it returns the comment "getting cached inverse". Otherwise, it calculates the inverse and stores it in the cache so that it may be accessed in later iterations or as needed.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()						# this calls the function getinverse to check for the presence of the inverse
	if(!is.null(inv)) {							# This part checks for the presence of the inverse and if it does exist, them get it and print out a message
		message("getting cached inverse")
		return(inv)
	}
	data <- x$get()								# If the inverse does not exist then the rest of the function calculates the inverse and commits it to the cache to be accessible
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
