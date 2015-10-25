## Marcos Bonfim - Oct-25-2015

## Create a list of functions to be use by cascheSolve


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

## Calculates de inverse and cache it for the future
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
