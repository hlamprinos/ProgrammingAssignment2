	## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	
	## Initialize the inverse property
	inversematrix <- NULL

	
	## Method to set the matrix
	set <- function(y) {
		x <<- y
		inversematrix <<- NULL
		
	}
	## Method to get the matrix and then return the matrix
	get <- function() x

	## Method to set the matrix and get th inverse of the matrix
	## And return the inverse property
	setinversematrix <- function(inverse) inversematrix <<- inverse
	getinversematrix <- function() inversematrix
	
	## Return a list of the methods
	list(set = set, get = get,
		 setinversematrix = setinversematrix,
		 getinversematrix = getinversematrix)
}
	

	## Compute the inverse of the special matrix returned by "makeCacheMatrix"
	## above. If the inverse has already been calculated (and the matrix has not
	## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x = matrix(), ...) {
	
	## Return a matrix that is the inverse of 'x'
	inversematrix <- x$getinversematrix()
	
	## Just return the inverse if its already set
	if(!is.null(inversematrix)) {
		message("getting cached data")
		return(inversematrix)
		
		
	}
	else {
	## Get the matrix from our object
		data <- x$get();
		
	##	Calculate the inverse using matrix and then set the inverse of the matrix
		inversematrix <- x$setinversematrix(solve(data));	
	}	
	## Return the matrix
	inversematrix;
	
}