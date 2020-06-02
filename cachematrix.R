## There are two functions below, which basically cache the inverse of the matrix. We assume that the matrix is invertible.
## First function makes the matrix, and the next one solves for the cache.
## The last part is a code demonstration for random square matrix, and the values can be changed.

## Here we make a cache matrix, meaning a matrix which can cache its inverse instead of computing it repeatedly

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse 
        getinverse = function() inv
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Here we solve for the invers of the matrix (the matrix capable of caching its inverse) that the above function returns, and return the same

cacheSolve <- function(x, ...) {
        inverse = x$getinverse()
        if (!is.null(inverse)){
                return(inverse)
        }
        tosolve = x$get()
        inverse = solve(tosolve, ...)
        x$setinverse(inverse)
        return(inverse)
}


## run this for a demonstration of the code:

# set.seed(1)
# r = rnorm(10000)
# y = matrix(r, nrow=100, ncol=100)
# z = makeCacheMatrix(y)
# cacheSolve(z)
