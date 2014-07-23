## Functions to cache the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache 
##its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse of the matrix to NULL
        inverse_of_matrix <- NULL
        
        ##function to set the matrix and initialize inverse to NULL
        set <- function(y) {
                x <<- y
                inverse_of_matrix <<- NULL
        }
        
        ## function to get the matrix
        get <- function() x
        
        ## function to set the inverse to be cached
        setinverse <- function(inverse) inverse_of_matrix <<- inverse
        
        ## function to get the cached inverse
        getinverse <- function() inverse_of_matrix
        
        ## return a list of the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## get the inverse of 'x'
        inverse_of_matrix <- x$getinverse()
        
        ##if the inverse is not NULL, get the cached data
        if(!is.null(inverse_of_matrix)) {
                message("getting cached data")
                return(inverse_of_matrix)
        }
        ## else get the matrix
        data <- x$get()
        
        ##calculate the inverse 
        inverse_of_matrix <- solve(data, ...)
        
        ## set the inverse so that it is cached
        x$setinverse(inverse_of_matrix)
        
        ## return the inverse
        inverse_of_matrix
}
