## Create a special "matrix" which caches its inverse. 
## Really, this special "matrix" is a list with getter and setter
## functions that are defined in an environment which contains
## a cached matrix inverse.

## Example usage
##     x = matrix(c(1:2,2:1),2,2)
##    cm = makeCacheMatrix(x)
##    cacheSolve(cm)


## Cache a matrix inverse in a parent environment
## and return a list of child functions which are aware 
## of parent environment and so can utilize the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # Cached value of matrix inverse 
    matrixInverse <- NULL
     
     # Gets the stored matrix
     get <- function() x

    # Set/Reset the matrix and clear any
    # caching of its inverse
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }

    # Store matrix inverse for re-use 
    setinverse <- function(inverse) {
         matrixInverse <<- inverse
    }

     # Gets the stored matrix inverse, will be NULL
     # if not previously cached.
    getinverse <- function() matrixInverse
     
     # Return a list of functions which have access to
     # parent environment's cached value.
     list(set = set, get = get, setinverse = setinverse, 
          getinverse = getinverse)
    
}


## Computes and returns a matrix inverse. 
## The inverse is computed and cached when retrieved for the first time.
## When retrieved subsequently, the inverse is pulled from cache
## saving the matrix inverse computation.
cacheSolve <- function(x) {
     m <- x$getinverse()
     
     # Check on availablity of cached inverse and return it if present
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # Otherwise, retrieve matrix, compute, store and return its inverse
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
