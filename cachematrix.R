## Andrew Marsh - 3/23/2016
## Programming in R - Assignment 2 - Write a pair of functions that cache the inverse of a matrix.

## The following Functions will be included:
    
##  1.  `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
##  2.  `cacheSolve`: This function computes the inverse of the special
##      "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated 
##      (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMtx = NULL
    set = function(y) {
        x <<- y
        invMtx <<- NULL
    }
    get <- function() x
    setinvMtx <- function(inverse) invMtx <<- inverse 
    getinvMtx <- function() invMtx
    list(set=set, get=get, setinvMtx=setinvMtx, getinvMtx=getinvMtx)
}


## This function (cacheSolve) computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve`
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMtx = x$getinvMtx()

    # Has the inverse already been calculated? Then provide a message and return the inverse.
    if (!is.null(invMtx)){
        message("getting cached data")
        return(invMtx)
    }
    
    # If the inverse has not been calculated, then calculate the inverse.
    mtx.data <- x$get()
    invMtx <- solve(mtx.data, ...)

    x$setinvMtx(invMtx)
    
    invMtx
}
