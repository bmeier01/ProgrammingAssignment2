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
    setmatrixinv <- function(solve) m <<- solve # this function is called by cacheSolve to compute the inverse of the matrix
    getmatrixinv <- function() m # this function provides the inverse of the matrix in cacheSolve if already calculated previously 
    list(set = set, get = get, # internal methods or functions
    setmatrixinv = setmatrixinv,
    getmatrixinv = getmatrixinv)
}


## This function computes the inverse of the matrix "makeCacheMatrix" after checking if it has not been calculated before
## if it has been calculated before it does not recalculate but returns it from cache.

cacheSolve <- function(x, ...) {
    m <- x$getmatrixinv() # obtain the inverse of the matrix from cache and store in m
    if(!is.null(m)) { # tests if m is not NULL. If this is TRUE, inverse has been calculated before and is consecutively diplayed
        message("getting cached data") # prints this message
        return(m)       # and returns inverse of the matrix (in this case from cache)
    }
    data <- x$get() # if m is NULL, this gets the matrix and stores it in data
    m <- solve(data, ...) # calculates the inverse of the matrix
    x$setmatrixinv(m) # stores the inverse of the matrix so that it can be retrieved from cache afterwards
    m #returns inverse of the matrix
}

