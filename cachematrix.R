## This file contains two functions to save time in calculating an inverse
## matrix by caching an output of the operation. It leverages the principle
## od lexical scoping in R.
## See === TEST === comment section below the code for testing the code.

## The first function makeCacheMatrix() outputs a list of functions that 
## manipulate with the matrix.

makeCacheMatrix <- function(original.matrix = matrix()) {
    inv.matrix <- NULL      # Inverse matrix is nulled at the beginning.
    fun.setmatrix <- function(y) {
        original.matrix <<- y   # Setting original matrix into parent environment,
        inv.matrix <<- NULL     # while inverse matrix is nulled even in parent.
    }
    fun.getmatrix <- function() original.matrix     # Outputs original in a function.
    fun.setsolve <- function(inver) {
        inv.matrix <<- inver        # Sets inverse matrix into parent environment.
    } 
    fun.getsolve <- function() inv.matrix   # Prepares to output inverted matrix
    list(setmatrix = fun.setmatrix, getmatrix = fun.getmatrix, 
         setsolve = fun.setsolve, getsolve = fun.getsolve)
    # Output is a list of functions with descriptive names.
}

## Second cacheSolve() function handles the inversion of the matrix.
## In case the operation happened before and is cached, the function
## gets the cached matrix and returns it.

cacheSolve <- function(x, ...) {
    # An argument 'x' is supposed to be a cached matrix or matrix to be cached.
    inv.matrix <- x$getsolve()  # It tried to get cached inverse matrix.
    if(!is.null(inv.matrix)) {  # In case inverse matrix exists in cache,
        message("Getting cached data...")
        return(inv.matrix)      # return it from cache with the message and finish.
    }
    data <- x$getmatrix()       # In case inverse matrix was not in cache, get original matrix
    inv.matrix <- solve(data, ...)  # and calculate inverse matrix out of it.
    x$setsolve(inv.matrix)      # Then CACHE the inverse matrix,
    inv.matrix                  # and return the inverse matrix into a consol.
}

# =====  TEST =====
## Test in the console after sourcing cachematrix.R:
##
## First we create a matrix and store it in 'myMatrix'
# > myMatrix <- matrix(stats::rnorm(16), nrow = 4, ncol = 4)

## Then we store caching function of the matrix into another object 'CachedMatrix'
#
# > CachedMatrix <- makeCacheMatrix(myMatrix)

## To create an inverse matrix, we call cacheSolve() function with 
## our 'CachedMatrix' as an argument
#
# > cacheSolve(CachedMatrix)

## When calling cacheSolve() for the first time, we get the inverse matrix only
## With another attempt...
#
# > cacheSolve(CachedMarix)

## ...we get message "Getting cached data..." before the printed matrix.
## This is the proof that caching works.




