
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <<- NULL    ##Initialize  the data structure for the inverse
        set <- function(y){                          ## Method for setting the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x                          ## Method for getting the matrix
        setinverse <- function() i <<- solve(x)      ## Method for calculating the inverse
        getinverse <- function() i                   ## Return the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        ## Return the matrix if it is alraedy set
        if(!is.null(i)){
                message("Getting cached data") 
                return(i)
        }
        data <- x$get()       ## Get the matrix from our object
        i <- solve(data, ...)
        x$setinverse(i)       ## Set the inverse of the object
        i
        
        ## Return a matrix that is the inverse of 'x'
}