## Implementation of Lexical scoping concept.

## Below function makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## It has nested functions to set and get matrix and inverse of matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv.mat <- NULL
        set <- function(y) {
                x <<- y
                inv.mat <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inv.mat <<- inv
        getInv <- function() inv.mat
        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}


## Below function cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x' either from cache, 
        ## or by using solve function for new matrix.
        inv.mat <- x$getInv()
        if(!is.null(inv.mat)) {
                message("getting cached data")
                return(inv.mat)
        }
        data <- x$get()
        inv.mat <- solve(data, ...)
        x$setInv(inv.mat)
        inv.mat
}
