## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The next 2 functions are used to cache the inverse os a matrix. 

## This function creates a special "matrix" object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y){
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) invmatrix <<- inversed
        getinverse <- function() invmatrix
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        datmatrix <- x$get()
        invm <- solve(datmatrix, ...)
        x$setinverse(invm)
        invm
}
