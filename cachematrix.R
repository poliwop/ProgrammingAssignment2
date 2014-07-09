## makeCacheMatrix makes a matrix object capable of caching its inverse.
## This object can then be passed to cacheSolve, which will return the inverse
## of the original matrix, without recomputing if the inverse was cached.


## Takes a matrix, returns a matrix object with the capability
## of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a cacheMatrix object (as created by makeCacheMatrix)
## and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
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
