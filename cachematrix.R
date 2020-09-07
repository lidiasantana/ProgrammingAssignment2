
## Function 1
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

## Function 2
## This function computes the inverse of the special "matrix" returned by
### makeCacheMatrix above. If the inverse has already been calculated
### (and the matrix has not changed), then the cacheSolve should retrieve the
### inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}


## testing
M <- matrix(sample(0:3,4),2,2)
M_cache <- makeCacheMatrix(M)
cacheSolve(M_cache)


