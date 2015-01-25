## Put comments here that give an overall description of what your
## functions do

## These functions are helpful in caching values for later use.
## They create the necessary functions to store and get the values at any point in time.

## Write a short comment describing this function
## This function defines 4 functions to be used in the process.

makeCacheMatrix <- function(x = matrix()) {
        ## This step sets m to null, defining it.
        m <- NULL
        
        ## This function set takes argument y and caches it in x. It also sets m to null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## This function returns the value of x
        get <- function() x
        
        ## This function stores the inverse in variable m
        setinverse <- function(inverse) m <<- inverse
        
        ## This function returns the inverse cached in variable m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function handles the cached values

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #3 Assignsvalue to m. If m is not null, then it return m. Meaning, if m has been cached before.
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
