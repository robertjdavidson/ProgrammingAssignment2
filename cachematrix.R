## Put comments here that give an overall description of what your
## functions do

## Based on makeVector example, it accepts a matrix and returns a "list"
## of functions that help with caching of a single matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Based on cachemean example, it accepts a cachedMatrix and returns either 
## the cached, solved version or it solves the matrix and returns it.
## Usage:
## foo <- makeCacheMatrix(myMatrix)
## bar <- cacheSolve(foo)
## Running it again will returned the cached answer as confirmed by the call to message()
## bar <- cacheSolve(foo)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
