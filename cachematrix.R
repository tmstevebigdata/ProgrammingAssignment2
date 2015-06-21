
## The function 'makeCacheMatrix' below is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mtrx <- NULL

    set <- function(y) {
        x <<- y
        mtrx <<- NULL
    }

    get <- function() x
    setmtrx <- function(inv) mtrx <<- inv
    getmtrx <- function() mtrx

    list(set = set, get = get,
         setmtrx = setmtrx,
         getmtrx = getmtrx)
}

## The function 'cacheSolve' below is a function that computes the inverse of the special "matrix" returned by function 'makeCacheMatrix' above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invmtrx <- x$getmtrx()

    if(!is.null(invmtrx)) {
       message("getting cached data")
       return(invmtrx)
    }

    data <- x$get()
    invmtrx <- solve(data, ...)
    x$setmtrx(invmtrx)

    invmtrx
}
