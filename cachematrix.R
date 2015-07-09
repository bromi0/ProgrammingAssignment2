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
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
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
    
    ## 
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
