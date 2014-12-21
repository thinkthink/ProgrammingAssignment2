## Caclulteing maxtrix inversion is costly computation
## Therefore, there may be some benefit to caching the inverse of a matrix instand ofcomputing it repeatedly
## This project inclued two function as below 

## MakeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. Set the value of matrix
## 2. Get the value of matrix
## 3. Set the value of inverse of matrix
## 4. Get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Check the inverse of matrix has already been calculated or not 
## If so, it gets the inverse of matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via the setmean function.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
