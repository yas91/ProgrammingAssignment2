# Matrix inversion is usually a costly computation and would benefit by caching
# the computation rather than haveing it done repeatedly.

 
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If true is returns the result and skips the
# computation. If not, it computes the inverse and caches the values. 

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        ## A matrix is returneed that is the inverse of 'x'
}

## Sample:
## > x = rbind(c(3, -3/5), c(-3/5, 3))
## > m = makeCacheMatrix(x)
## > m$get()
##        [,1] [,2]
##[1,]  3.0 -0.6
##[2,] -0.6  3.0

## No cache in the first run
## > cacheSolve(m)
##           [,1]       [,2]
##[1,] 0.34722222 0.06944444
##[2,] 0.06944444 0.34722222

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]       [,2]
##[1,] 0.34722222 0.06944444
##[2,] 0.06944444 0.34722222
## > 
