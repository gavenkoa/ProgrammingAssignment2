## Put comments here that give an overall description of what your
## functions do

## Constract wrapper for matrix object which hold cached metrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cache <<- inverse
    getinverse <- function() cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return matrix inverse for wrapped by makeCacheMatrix object. Cache the
## result for future calls.

cacheSolve <- function(x, ...) {
    cache <- x$getinverse()
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    data <- x$get()
    cache <- solve(data, ...)
    x$setinverse(cache)
    cache
}
