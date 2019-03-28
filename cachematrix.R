## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: 
## This function makeCacheMatrix 
## creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function:
## This function cacheSolve calculates the inverse of the special matrix retuned by
## the makeCacheMatrix funciton abouve. However, it first checks to see if the  
## inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting your cached data master")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
