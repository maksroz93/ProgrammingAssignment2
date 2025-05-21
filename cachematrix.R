## Functions below are used to store matrix and it's inverse in memory and 
## provide possibility to reuse inverse in future calculations. 

## makeCacheMatrix creates a matrix object that can cache it's inverse. It also
## contains the set of functions to manually set a new matrix (set()), retrieve 
## the current matrix (get()), get and set the inversed matrix (get/setinverse).

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


## Recieves a matrix object created by makeCacheMatrix, checks whether inverse
## has already been calculated. If yes - returns it from chache, otherwise - 
## solve the matrix and store it in cache and then return.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
