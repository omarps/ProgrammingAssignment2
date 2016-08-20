## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Creates a cached inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Resolves the inverse of a matrix, cached or computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
