
## This program will create and cache the inverse of a square matrix.

##This first function, makeCacheMatrix, creates a special "vector".
##This special "vector" is a list containing a function to:
## - set a value of the vector
## - get the value of the vector
## - set the value of the inverse of the matrix
## - get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function, cacheSolve, calculates the inverse 
## of the special "vector" created by function makeCacheMatrix. 
## If first checks to see whether the inverse of the matrix has already been calculated. 
## If so, it gets the inverse of the matrix from the cache and skips 
## the computation and gives the inverse of 
## the matrix with the comments "getting cached data". 
## Otherwise, it will calculate the inverse of the matrix and 
## sets the value of the inverse of the
## matrix in the cache via setinv function.

cacheSolve <- function(x, ...) {
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv
}
        

