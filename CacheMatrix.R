## This function creates a 'matrix' to cache its inverse
makeCacheMatrix <- function(x = matrix ()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
  }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}

## This function will check for a cached version already inversed, if the matrix hasn't changed.  Otherwise it will cache its inverse.
cachesolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
            message("getting cached data")
            return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
