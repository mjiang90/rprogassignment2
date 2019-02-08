## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the inverse property 
        i <- NULL
        
        ## Method to set the matrix
        set <- function (matrix) {
                x <<- matrix
                i <<- NULL
        }
        
        ## Method to get the matrix
        get <- function() {
                ## return the matrix 
                x
        }
        
        ## Method to set the inverse of the matrix
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        ## method to get the inverse of the matrix
        getinverse <- function() {
                ## return the inverse property
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by 
## "makeCacheMatrix" above. If the inverse has already been 
## calculated (and the matrix has not changed), then the "cachesolve" 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## just return the inverse if its already set
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## get the matrix from our object
        data <- x$get()
        
        ## calculate the inverse using matrix multiplication
        i <- solve(data, ...)
        
        ## set the inverse to the object
        x$setinverse(i)
        
        ## return the matrix
        i
}
