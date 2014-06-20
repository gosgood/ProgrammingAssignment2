# These functions use the lexical scoping rules in R to store a matrix and
# cache its inverse once it is computed.  Then, if the inverse is requested
# again, the cached version is returned instead of recomputing it
# (a potentially expensive operation).

# makeCacheMatrix essentially "wraps" a matrix with a list of 4 functions
# that get and set the matrix, as well as get and set the inverse of the 
# matrix
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    # functions to set and get the matrix itself.  If we set the matrix, 
    # the cached inverse is no longer valid and must be reset.
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    # functions to set and get the matrix inverse.
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    # return the list of functions that can be used to manipulate the matrix
    # and its inverse.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


# cacheSolve computes the inverse of a matrix and caches it so that if it is
# requested again, the cached version can be returned instead of recomputing
# it.
cacheSolve <- function(x, ...) 
{
    # Return a matrix that is the inverse of 'x'
    # Check to see if the inverse has already been computed.  If it has, 
    # return the cached version.
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    
    # If not, retrieve the original matrix and call the solve function to
    # compute the inverse.  Then, cache the inverse and return it.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
