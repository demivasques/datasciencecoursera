## These functions are caching functions for optimizing long computations

## First function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x << y
                m << NULL
        }
        get <- function() x
        setmatrix <- function(solve) x
        getmatrix <- function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Second function checks if the inverse has been calculated. 
## If so, it retrieves from the cache.
## If not, it computes the inverse from the matrix in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
