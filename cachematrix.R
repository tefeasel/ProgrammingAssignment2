## These two functions will ultimately take a matrix and calculate its inverse.
## The first function has the ability to cache the inverse of the matrix, while the 
## second can retrieve the cached matrix (or even calculate it). By caching the inverted matrix,
## we can potentiall reduce the cost of computation, of which calculating the inverse of a matrix
## can be high.

## First function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) i <<- inverse
        getInv <- function() i
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## Retrieves cached matrix from makeCacheMatrix (if stored in makeCacheMatrix 
## object environment), Otherwise it will invert the matrix from makeCacheMatrix.
cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("retreiving cached data")
                return(i)
        }
        
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setInv(i)
        i
}

##Test
z <- matrix(1:4, 2, 2)
mine <- makeCacheMatrix(z)
cacheSolve(mine)