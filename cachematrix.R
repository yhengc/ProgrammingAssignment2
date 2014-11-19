## The two functions below are created to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It actually returns a list of four elements (set, get, setsolve, getsolve).

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
## It first checks if the desirable value was already cached.
## If so, it returns the cached value. If not, it calculates and returns the
## value.

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