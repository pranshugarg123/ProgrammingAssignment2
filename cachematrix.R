## Overall description:
## The following functions are designed to cache the inverse of a matrix
## to optimize computations. The makeCacheMatrix function creates a special
## matrix object that can cache its inverse, while the cacheSolve function 
## computes the inverse of this matrix and retrieves it from the cache if 
## it has already been calculated.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse to NULL
    
    ## Function to set the value of the matrix
    set <- function(y) {
        x <<- y  # Assign the new matrix to the variable x
        inv <<- NULL  # Reset the cached inverse when the matrix is changed
    }
    
    ## Function to get the value of the matrix
    get <- function() {
        x  # Return the matrix
    }
    
    ## Function to set the cached inverse
    setInverse <- function(inverse) {
        inv <<- inverse  # Cache the calculated inverse
    }
    
    ## Function to get the cached inverse
    getInverse <- function() {
        inv  # Return the cached inverse
    }
    
    ## Return a list of functions to interact with the matrix object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get the cached inverse
    
    ## Check if the inverse is already cached
    if (!is.null(inv)) {
        message("getting cached data")  # Message indicating cached data is used
        return(inv)  # Return the cached inverse
    }
    
    ## Get the matrix and calculate the inverse
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse using the solve function
    x$setInverse(inv)  # Cache the computed inverse
    
    ## Return the computed inverse
    inv
}
