## These functions build on the concepts of lexical scoping. The idea is 
## is to demonstrate a method to save computer processing time for 
## costly calculations (like inverse matrices). The first function will
## will create a matrix that caches the calculation of it's own inverse
## and the second function will return the inverse matrix from from cache
## if it exists and has not changed. 

## makeCacheMatrix is a function which creates a matrix object that 
## can cache it's own inverse 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the special 
## matrix created in makeCacheMatrix. If the inverse value has been
## calculated and has not changed, cacheSolve will return the stored
## cached value of the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if the inverse
        ## matrix cache is not null. 
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If the cache is empty, solve for the inverse of the matrix
        ## and return the result
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
