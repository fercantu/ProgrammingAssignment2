# Matrix inversion is a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly. 
# The following two functions cache the inverse of a given matrix.


# This function creates a special "matrix" object that can cache its inverse.

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


# This function computes the inverse of the special "matrix" returned by the
# function above. If the inverse has already been calculated (and the matrix
# has not changed), then this function should retrieve the inverse from the cache.

# Note: we assume that the matrix provided is always inversible and the function
# solve(x) will not produce an error.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}