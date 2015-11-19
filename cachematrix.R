## Put comments here that give an overall description of what your
## functions do
# The following functions are:
# 1. makeCacheMatrix: This function creates a special "matrix" object that
# can cache its inverse
# 2. cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cacheSolve should retrieve the inverse
# from the cache.

## Write a short comment describing this function
# This function named makeCacheMatrix() creates a special "matrix",
# which is really a list containing four functions F1 through F4
makeCacheMatrix <- function(x = matrix())
{
    i <- NULL
    set <- function(y)  # F1: to set the value of the matrix
    {
        x <<- y
        i <<- NULL  # initializes inverse to NULL while caching the matrix
    }
    get <- function() x # F2: to get the cached value of the matrix
    setinv <- function(solve) i <<- solve # F3: to set the value of the inverse
    getinv <- function() i # F4: to get the cached value of the inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    # to return the list of four functions (F1 through F4)
}

## Write a short comment describing this function
# This funcion named cacheSolve() calculates the inverse of a special "matrix"
# created with the function makeCacheMatrix()
cacheSolve <- function(x, ...)
{
    i <- x$getinv() # fetches the cached value of matrix inverse
    if(!is.null(i)) 
    {
        # if the fetched value is non-null => inverse was calculated before
        message("getting cached matrix inverse")
        return(i) # return the cached value of inverse
    }
    mat <- x$get() # if inverse is null, fetch the cached matrix 
    i <- solve(mat, ...) # evaluate the inverse of the cached matrix
    x$setinv(i) # cache the matrix inverse for future use
    i ## Return a matrix that is the inverse of 'x'
}        
