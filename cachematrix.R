## These functions will take a matrix (will only accept an invertible one)
## and cache the inverse of it. If the function is called again and the
## matrix has not changed it will retrun the inverse from the cache


## This function does the storing of the inverse of a matrix into cache
## To start off run your matrix into makeChacheMatrix("your matrix")

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will first check to see if the inverse of your matrix
## is already cached in the makeCacheMatrix. If not, it will calculate
## the inverse of the matrix and store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
