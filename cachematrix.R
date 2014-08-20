## makeCacheMatrix and cacheSolve allow for a matrix object and its inverse to 
## be cached in memory for quick retrieval. If no inverse exists, it will be 
## generated using the solve() function

## makeCacheMatrix creates object containing invertible matrix and functions for
## getting and setting the value of the matrix and its inverse
##
## Args:
##   x: an invertible matrix. Default is empty matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve determines if inverse matrix has been solved for makeCacheMatrix 
## object and either returns cached inverse or solves inverse
##
## Args:
##  x: a makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}
