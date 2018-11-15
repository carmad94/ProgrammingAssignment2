
## makeCacheMatrix accepts a matrix as a parameter and returns a list of functions. 
## The functions are:
## setMatrix -> initializes and cache a matrix inside an environment
## getMatrix -> gets the cached matrix from the environment
## setInverse -> initializes and cache an inverse of a matrix inside an environment
## getInverse -> gets the cached inverse of matrix from the environment

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    setMatrix <- function(y){
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(y) inverse <<- y
    getInverse <- function() inverse
    list(setMatrix=setMatrix, getMatrix=getMatrix,
        setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve is a function which accepts a list of functions 
## generated from the makeCacheMatrix function and returns the inverse of a matrix
## It first calls the getInverse function. 
## If it returns null then it will compute the inverse of the matrix set in the cache.
##     It will then call the setInverse function to cache the computed inverse
## Else it will return the cached inverse of a matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    mtrx <- x$getMatrix()
    inv <- solve(mtrx, ...)
    x$setInverse(inv)
    inv
}
