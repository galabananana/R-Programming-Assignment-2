## The following code is to cache the inverse of a matrix 

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverse <<- inverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above
## If the inverse has already been calculated and the matrix has not changed,
## then the casheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)){
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
