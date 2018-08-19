## R functions to cache data in a different environment for performance purposes and training

## cache the matrix and its inverse in a different environment
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns the inverse of the matrix
## caches it to improve the performance
cacheSolve <- function(x, ...) {
    data <- x$get()
    if(x == data) {
        im <- x$getinverse()
        if(!is.null(im)) {
            message("getting inverse matrix from cache")
            return(im)
        }
    }
    im <- solve(data)
    x$setinverse(im)
    im
}
