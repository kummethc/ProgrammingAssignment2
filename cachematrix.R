## First function(makeCacheMatrix) creates a matrix which is used to cache the inverse of the matrix. 
## second function(cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## solve function is used to calculate the inverse. 

## below function - makeCacheMatrix creates a matrix which can be used to cache the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y= matrix()) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}                  


## Below function - cacheSolve calculates inverse of the matrix. If the inverse is already calculated, get from the cache else calculates the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}