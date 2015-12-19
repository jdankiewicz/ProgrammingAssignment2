## Used together makeCacheMatrix and cacheSolve solve the inverse of a matrix,
## the inverse is then stord in cache, from which the results
## can be retrieved.This procedure eliminates the need for a costly computation.

## makeCachematrix creates a list of functions that store the inverse of a
## a matrix in cache. X and M can be retrieved from outside the function. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Retrieve the matrix
        get <- function() x
        ## Set the Inverse of the matrix
        setInv <- function(solve) m <<- solve
        ## Get the Inverse of the matrix
        getInv <- function() m
        ## List of methods in the function
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}      
        
        
## cacheSolve first checks to see if the inverse in available in cache. If  it
## available in returns the solution. If the inverse is not available in the 
## cache the function calculates it and stores it in setInv.
        cacheSolve <- function(x, ...) {
## Check availability of inverse and retrieve from cache
                m <- x$getInv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                mat <- x$get()
                m <- solve(mat, ...)
                x$setInv(m)
                m
}

