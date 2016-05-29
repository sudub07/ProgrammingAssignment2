## The objective here is to cache the inverse operation of the matrix 
## in order to decrease the computation cost of the matrix inverse operation

## The makeCacheMatrix function creates a special matrix object that can cache 
## it's inverse. This function contains 4 functions for setting and retreiving values

makeCacheMatrix <- function(matr = matrix()) 
{
    ##stores inverse of a matrix
    inverse <- NULL  
    
    ## setter method to initialize the objects
    ## The '<<-' operator is used to initialize objects from a different environment
    setMatrix <- function(y) {
        matr <<- y
        inverse <<- NULL
    }
    
    ## getter method to get the value of the input matrix
    getMatrix <- function() matr
    
    ## setter method to set value for inverse matrix object
    setMatInverse <- function(inv) inverse <<- inv
    
    ## getter method to retreive the value for the inverse of a matrix
    getMatInverse <- function() inverse
    
    ## The output is a list of 4 functions to be used in cacheSolve() function
    list(setMatrix = setMatrix, getMatrix = getMatrix, setMAtInverse = setMatInverse, 
         getMatInverse = getMatInverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated and the matrix 
## has not changed, then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getMatInverse()    ## gets cached value, if present
    
    ## checks if there is a value retreived and returns it.
    if(!is.null(inv)) 
    {
        message("Inverse of the matrix exists. Getting cached data")
        return(inv)             ## Program stops executing on hitting return
    }
    
    ## Since the inverse was not cached, it will be calculated.
    ## gets the matrix for which the inverse is to be calculated.
    data.matrix <- x$getMatrix()
    
    ## calculates inverse of the matrix and cache's the value
    inv <- solve(data.matrix, ...)
    x$setMAtInverse(inv)
    inv
}
