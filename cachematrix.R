## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 
makeCacheMatrix <- function(x = matrix()) {
       
}


## Return inverse of a square matrix. Get result from 
## cache if present, if not calculate, insert to cache 
## before returning result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Basic sanity checks for input
        stopifnot(!(is.na(x) | is.null(x) | !is.matrix(x))

        ## Try get result from cache, return result if found
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
              message('Return inverse from cache')
              return(inverse)
        }
    
        ## Result not in cache, calculate one, 
        ## store in cache then return result
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

# Basic tests for validation
test <- function() {


}

