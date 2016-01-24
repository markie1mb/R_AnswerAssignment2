## CacheSolve.r contains 2 functions
## 1. makeCacheMatrix(matrix)
##    create a list of functions in a specific matrix in order to cache the 
##    Inverse of that matrix
##    4 functions will be created on the matrix
##      set:        Sets the matrix itsself
##      get:        Gets the matrix
##      setsolve:   sets the inverse of the matrix
##      getsolve:   gets the inverse of the matrix
##
## 2. cacheSolve(input must be the result of makeCacheMatrix)
##    get the cached inverse of the matrix created with makeCacheMatrix
##    If the Inverse cache does'n exist: create the cache of the inverse of 
##    the matrix

## a matrix I used to test:  matrix(c(1,3,6,3,7,8,9,12,1,4,3,7,6,2,5,3),4,4)

## create a list which contains the data of a matrix together with
## a list of 4 functions

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


##    get the cached inverse of the matrix created with makeCacheMatrix
##    If the Inverse cache does'n exist: create the cache of the inverse of 
##    the matrix

cacheSolve <- function(x, ...) {
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