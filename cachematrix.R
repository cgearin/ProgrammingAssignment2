## Function creates a special vector which is a list containing 
## a function to set the matrix, get the matrix, create the inverse of the matrix,
## and retrieve the inverse.



makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x			##gets matrix 
	setinverse <- function(solve) m <<- x   ##calls 'solve' to invert matrix from get
	getinv <- function() m			##retrieves inverse of matrix
	list(set = set, get = get, setinverse = setinverse, getinv = getinv)
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix. If inverse has already been calculated and matrix not changed,
## then inverse retrieved from cache.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {                               ## if m has inverse stored it is retrieved from cache
 		message("getting cached data")       
		return(m)
	}
	data <- x$get()                                 ## if inverse not stored, this will compute it
	m <- solve(data, ...)	
	x$setinverse(m)
	m
}

