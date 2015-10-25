## Marcos Bonfim - Oct-25-2015

## This function creates a list of functions to be use by cascheSolve
## The variable inverso is set to NULL at first and once the Inverse matrix is
## calculated once, this variable is used to store the result

makeCacheMatrix <- function(x = matrix()) {
        inverso <- NULL
        set <- function(y) {
                x <<- y
                inverso <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverso <<- solve
        getinv <- function() inverso
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function uses the functions defined above to calculate the inverse of a matrix
## It first tests if the variable Inverso has a value, if so, just get it back and avoid 
## the recalculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverso <- x$getinv()
        if(!is.null(inverso)) {
                message("getting cached data")
                return(inverso)
        }
        data <- x$get()
        inverso <- solve(data, ...)
        x$setinv(inverso)
        inverso
}
