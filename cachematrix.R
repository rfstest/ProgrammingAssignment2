## This module allows using matrices with their cached inverses.
##      A wrapper-object, which stores a matrix with its inverse is created
##      with the makeCacheMatrix() function; the inverse can be computed with
##      the cacheSolve() function.

# Creates an object, which represents a matrix and its cached inverse
makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL
    set <- function(y) {
        x <<- y
        cached.inverse <<- NULL  # wipe the cache on new matrix
    }
    get <- function() x
    set.inverse <- function(inverse) cached.inverse <<- inverse
    get.inverse <- function() cached.inverse
    list(set = set, get = get,
        set.inverse = set.inverse,
        get.inverse = get.inverse)
}


# Returns the cached inverse of a matrix;
#   if the inverse is not computed yet, it is computed and cached.
cacheSolve <- function(x, ...) {
    cached.inverse <- x$get.inverse()
    if(is.null(cached.inverse)) { # inverse not computed yet
        m <- x$get()
        cached.inverse <- solve(m, ...)
        x$set.inverse(cached.inverse) # cache inverse
    }
    cached.inverse
}
