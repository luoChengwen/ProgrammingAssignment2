## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function (makeCacheMatrix) creates a matrix, and get its inverse matrix if there is

# set the value of a matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function (y) {
                x <<- y
                invers <- NULL
        }
        get <- function () x
        setinvers <- function (matInverse) invers <<- matInverse
        getinvers <- function () invers
        
        list (set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}


## This function (cacheSolve) calculates the inverse of matrix if it has not yet been calculated

cacheSolve <- function(x, ...) {
        invers <- x$getinvers ()
        if (!is.null(invers)) {
                message('getting the cached inverse matrix')
                return(invers)
        }
        data <- x$get()
        invers <- solve(data,...)
        x$setinvers(invers)
        invers ## Return a matrix that is the inverse of 'x'
}
