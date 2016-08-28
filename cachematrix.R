## Implementation of Lexical scoping concept

## Below function creates Matrix 

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


## returns inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
