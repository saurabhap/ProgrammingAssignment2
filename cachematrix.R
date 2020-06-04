##The makeCacheMatrix function takes a matrix object as an argument and builds a special matrix object that can 
## save a matrix and its inverse.


## setm() receives matrix as an argument and stores it in x object
## getm() returns stored object
## setinv(inv) -> Receives inverse of matrix as an argument and saves it inside the object (cache).
## getinv() -> Returns the inverse of the matrix if it's saved inside the object (cache).

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
         setm<- function(y){
                 x <<- y
                 inverse <<- NULL
         }
        getm <- function() x
        setinv <- function(inv) inverse <- inv
        getinv <- function() inverse
        list(set= setm, get= getm, setinverse= setinv, getinverse= getinv)
}

## The cacheSolve function returns the inverse of the special matrix object inside the makeCache function.
## If inverse is already calculated, then it return cache value. Otherwise, it gets data and calculates inverse
## then calculated inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
