## 'makeCacheMatrix' stores the functions that set the matrix and
##      caches its inverse (calculated by 'cacheSolve');
## 'cacheSolve' checks if the inverse matrix was already calculated
##      if not, the inverse is calculated and stored at 's'.


## Function to stores a list of three functions:
##      get: get (returns) the matrix 'x'
##      setSolve: set (stores) the inverse of 'x' at 's'
##      getSolve: get (returns) the inverse of 'x' stored at 's'
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        ## If the inverse matrix was already calculated
        ## (and stored at 's')
        ## the function returns the cached data.
        s <- x$getSolve()
        if(!is.null(s)) {
                message("Getting cached data:")
                return(s)
        }
        ## otherwise, it calculates the inverse matrix
        ## and stores it at 's'
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}

###########################################
## A simple script to test the functions:
## 
## source("cachematrix.R")
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## x$get()
## ## (the original matrix will be shown)...
## cacheSolve(x)
## ## (the inverse matrix will be shown)...
## cacheSolve(x)
## ## (the cached inverse matrix will be shown)...
## 
## Suggested by Al Warren
## https://class.coursera.org/rprog-015/forum/thread?thread_id=447
##
###########################################
