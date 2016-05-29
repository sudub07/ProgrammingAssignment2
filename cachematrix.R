## The objective here is to cache the inverse operation of the matrix 
## in order to decrease the computation cost of the matrix inverse operation

## The makeCacheMatrix function creates a special matrix object
## that can cache it's inverse

makeCacheMatrix <- function(matr = matrix()) 
{
    inverse <- NULL
    set <- function(y) {
        matr <<- y
        inverse <<- NULL
    }
    get <- function() matr
    
    setInverse <- function(inv) inverse <<- inv
    
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
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data.matrix <- x$get()
    inv <- solve(data.matrix, ...)
    x$setInverse(inv)
    inv
}
