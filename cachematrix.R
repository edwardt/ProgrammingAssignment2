## Put comments here that give an overall description of what your
## functions do

## Create a vector of function that store/get
## calculated result in global environment
## Function only accepts to matrix type

## 
makeCacheMatrix <- function(x = matrix()) {
        ## Caching scheme uses variable reference as the lookup key.
        ## Since R ties reference variable is defining scope in lexical scoping,
        ## which means that value(the inverse) has to be enclosed within variable 
        ## This also means although 2 variables pointing to matrix 
        ## with same valued, they are treated as seperate inverse calculations
        ## So this is not a "true cache", 

        inverse <- NULL

        # normal cache function: get/set
  
        # update matrix value
        set <- function(newMatrix, inverse) {
             ensure_square_matrix(newMatrix)
             x<<-newMatrix
             inverse <<- NULL #new matrix invalidates old cache value
        }
        # get current matrix 
        get <- function() x

        # hard set inverse to X
        setInverse <- function(x) {
             ensure_square_matrix(x)
	     inverse <<- x
        }

        # return what's in cache
        getInverse <- function() inverse

        list(get=get, set=set,
             getInverse=getInverse, setInverse=setInverse)
}


## Return inverse of a square matrix. Get result from 
## cache if present, if not calculate, insert to cache 
## before returning result.

cacheSolve <- function(x, ...) {
        
        ## Basic sanity checks for input
        ensure(is.null(x), 'Input is null')
        ensure(is.na(x), 'Input value is missing')
        ensure(is.matrix(x), 'Input is not a matrix')
        ensure_square_matrix(x)

        ## Try get result from cache, return result if found
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
              print('Return inverse from cache')
              return(inverse)
        }

        ## Result is not in cache, calculate one, 
        ## store in cache then return result
        data <- x$get()

        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}

ensure_square_matrix <-function(x) {
         d <- dim(x)
         ensure(!(d[1] == d[2]), 'Not a square matrix')
}

ensure <- function(cond, message) {
          ifelse(cond, stop(message), TRUE)       
}

# Basic tests for validation
test <- function() {
     m <- matrix(1:4, nrow=2, ncol=2)
     n <- matrix(c(0,5,99,66), nrow=2, ncol=2)
     print(m)
     amatrix <- makeCacheMatrix(m)
     bmatrix <- makeCacheMatrix(m)
     cmatrix <- amatrix
     print('Is amatrix$get identical to original matrix?')
     print(identical(m, amatrix$get()))
     ar<- cacheSolve(amatrix)
     print('Inverse of amatrix: should not from cache')
     print(ar)
     print('Get amatrix inverse again.. should be null')
     print(amatrix$getInverse())
     print('cmatrix amatrix share same reference. Inverse of cmatrix be from cache')
     cr1<-cmatrix$getInverse()
     print(cr1)

     print('Inverse of bmatrix be NULL should not from cache')
     br1<-bmatrix$getInverse()
     print(br1)
     print('Set amatrix: ')
     print(n)
     amatrix$set(n)
     print('Is amatrix$get identical to new  matrix?')
     print(identical(n, amatrix$get()))
     print('Is amatrix inverser cache should be null(invalidated)')
     print(amatrix$getInverse())
     print('Inverse of amatrix: should not from cache')
     ar <- cacheSolve(amatrix)
     print(ar)
     print('Inverse of amatrix just calculated: now should be from cache')
     amatrix$getInverse()
     print('Inverse of cmatrix(same referecence to amatrix): should be from cache and has latest value')
     cr2 <- cmatrix$getInverse()
     print('Value of cmatrix no longer same as previous one. Still has old value?')
     print(identical(cr1, cr2))
     #cacheSolve(cmatrix)
     print('Inverse of bmatrix be NULL should not from cache')
     br2<-bmatrix$getInverse()
     print(br2)
     print('bmatrix has not changed')
     print(identical(br1,br2))
     print('Get bmatrix again: should be from cache')
     print(cacheSolve(bmatrix))

}

