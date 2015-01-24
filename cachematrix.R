## These functions are used to cache the inverse of a matrix rather than computing it every time
## If the contents of a vector are not changing, it may make sense to cache i.e. the value of the mean
## especially when it may take long to compute
## therefore when we need it again, it can be looked up in the cache rather than being recalculated

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


## This function computes the inverse of the matrix "makeCacheMatrix" above after checking if it has not been calculated before

cacheSolve <- function(x, ...) {
    m <- x$getmatrixinv() # gets the inverse of the matrix into m
    if(!is.null(m)) { # tests if m is not NULL, if this is TRUE, inverse has been calculated before and is diplayed
        message("getting cached data")
        return(m)       #returns inverse of the matrix
    }
    data <- x$get() # gets the matrix and stores it in data
    m <- solve(data, ...) # calculates the inverse of the matrix
    x$setmatrixinv(m)
    m #returns inverse of the matrix
}

