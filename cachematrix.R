## The objective here is to cache the inverse operation of the matrix 
## in order to decrease the computation cost of the matrix inverse operation

## The makeCacheMatrix function creates a special matrix object
## that can cache it's inverse

makeCacheMatrix <- function(matr = matrix()) 
{
    inverse <- NULL  ##stores inverse of a matrix
    
    ## setter method to initialize the objects
    set <- function(y) {
        matr <<- y
        inverse <<- NULL
    }
    
    ## getter method to get the value of the input matrix
    get <- function() matr
    
    ## setter method to set value for inverse matrix object
    setInverse <- function(inv) inverse <<- inv
    
    ## getter method to retreive the value for the inverse of a matrix
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()  ## gets cached value, if present
    ## checks if there is a value retreived and returns it
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    ## gets the matrix for which the inverse is to be calculated.
    data.matrix <- x$get()
    
    ## calculates inverse of the matrix and cache's the value
    inv <- solve(data.matrix, ...)
    x$setInverse(inv)
    inv
}
