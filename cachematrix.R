## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	set_inverse <-function(inverse) m <<- inverse
	get_inverse <-function() m
	list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function
## Calculates inverse if it doesn't exist, otherwise retrieves cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m

}
