## The two functions below are created to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four elements (set, get, setsolve, getsolve).
## It creates an object with four "methods" that can be accessed or changed
## by the second function cacheSolve() below.

makeCacheMatrix <- function(x = matrix()) {
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

## This function applies to the object created using makeCacheMatrix().
## It first checks if the desirable value (the inverse of the matrix) was 
## already cached.
## If so, it returns the cached value. A message will pop up to tell you that
## the value was not re-calculated but just fetched from the cache.
## If not, it calculates and returns the value. No message will display.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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