## The functions implement the mechanism to store calculated
## value of the inverse for the matrix so we don't have to recalculate
## it. They use enclosing environment of nested list of
## functions to store this value (variable m)

## Function returns a list of functions, in their environments
## will be stored value of matrix 'x'. 
## set -- sets the new value of matrix 'x'
## get -- retrieves the current stored matrix
## setinverse -- sets the cached inverse value
## getinverse -- retrieves the cached inverse value.

makeCacheMatrix <- function(x = matrix()) {
    ## place for storing cached attribute of x (inverse)
    m <- NULL
    
    set <- function(y) {
        ## operator <<- searches for a variable
        ## in enclosed environment, in this case
        ## the 'x' from function definiton
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    ## stores inverse value in cache
    setinverse <- function(inverse) m <<- inverse
    ## retrieves stored value from cache
    getinverse <- function() m
    
    ## returns a list of 4 functions to operate with
    ## this special matrix implementation which can be used
    ## like yourvariable$getinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve uses the special helper function 
## makeCacheMatrix to store, essentially, pairs of 
## matrices and their inverse values.
## When it is called on the same object, on which it was called
## before, function retrieves the stored value, skipping
## computation.
## The 'x' parameter should be the special list created with
## makeCacheMatrix

cacheSolve <- function(x, ...) {
    
    ## Request into cache inside 'x' environment
    m <- x$getinverse()
    
    ## if the cache exist, return the cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## otherwise, retrieve the matrix itself
    ## and use function solve to calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    ## store value in cache
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
