## These function are used to cache the inverse of a matrix rather than computing it
## If the contents of a vector are not changing, it may make sense to cache i.e. the value of the mean, especially when it may take long to compute the mean, so that when we need it again, it can be looked up in the cache rather than recomputed

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    } # set the parameters of the matrix
    get <- function() x # get the value of the matrix
    setmatrixinv <- function(solve) m <<- solve # set the inverse of the matrix
    getmatrixinv <- function() m # get the inverse of the matrix
    list(set = set, get = get,
    setmatrixinv = setmatrixinv,
    getmatrixinv = getmatrixinv)
}


## This function computes the inverse of the matrix "makeCacheMatrix" above

cacheSolve <- function(x, ...) {
    m <- x$getmatrixinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)       ## Return a matrix that is the inverse of 'x'
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrixinv(m)
    m
}

