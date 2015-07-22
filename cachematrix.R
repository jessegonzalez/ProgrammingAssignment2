## makeCacheMatrix and cacheSolve are used to cache the result of finding the inverse of a matrix
## which can be computationally intensive.
##
## Example Usage:
##
## tmp_matrix <- matrix(c(0,1,3,1,43,2,3,6,9), nrow=3, ncol=3)
## cache_matrix <- makeCacheMatrix(tmp_matrix)
## cacheSolve(cache_matrix)
## cacheSolve(cache_matrix)

## makeCacheMatrix creates a special "matrix", which is really a list of functions to
## get and set the value of the matrix
## get and set the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## get the value of the matrix
    get <- function() {
        x
    }

    ## set the value of the solved matrix
    setSolved <- function(solve) {
        m <<- solve
    }

    ## get the value of the solved matrix
    getSolved <- function() {
        m
    }

    list(set = set, get = get,
         setSolved = setSolved,
         getSolved = getSolved)
}


## The following function returns the inverse of the matrix created with the above function makeCacheMatrix().
## It first checks to see if the matrix inverse been calculated with getSolved.
## If that result is not NULL, it gets and returns the result from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value in the cache via the setSolved function,
##   then returns the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolved()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## Get the matrix and solve
    data <- x$get()
    m <- solve(data, ...)

    ## update the cache and return the result
    x$setSolved(m)
    m
}
