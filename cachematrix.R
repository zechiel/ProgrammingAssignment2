## Two functions that allow generating and inverting a matrix data type that
## caches its inverse.

## Function to generate the caching matrix data type. The return value
## is a list of four getter and setter functions, which are the only way
## to access the matrix data.
## The matrix and later its inverse are stored in the local environment
## of the generating function. This environment is not destroyed
## as long as the list of getter and setter functions
## is still in use (because of lexical scoping).
##
## Example use:
## cm <- makeCacheMatrix( matrix( c(1, -1, -1, 1), nrow = 2, ncol = 2) )

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ## The next two functions will only be used by cacheSolve():
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function returns the inverse of a matrix as returned by
## makeCacheMatrix. The inverse is only calculated when the matrix itself
## has been freshly set, otherwise the cached inverse is returned.
##
## Example use:
## cacheSolve(cm)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv}
