
##makeCachematrix: creating a special matrix:
makeCachematrix <- function(x = matrix ()) {

	inv = NULL   ##setting the variable inv to null
	set <- function(a) {
		x <<- a
		inv <<- NULL
	}
	get <- function() x    ##setting  function to get method
	
	setinv <- function(inverse) inv <<- inverse  ##setting the function with inverse  in setinv variable
	getinv  <- function() inv
	list(set = set, get = get,
	     setinv  = setinv ,
	     getinv  = getinv )
}
## get the `matrix` from the previously function and show its inverse using solve function
cachesolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) { ## if variable inv is not null, return inv
		message("getting cached data")
		return(inv)
	}
	mat.data <- x$get()
	inv <- solve(mat.data, ...)
	x$setinv(inv) ##setting inverse on x
	return(inv)
}
