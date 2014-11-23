## Extends the Matrix object to cache its inverse so it is only computed once.
## Per assignment instructions: assume that the matrix supplied is always invertible.

## Stores a matrix and it's inverse (once the inverse has been computed).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a given matrix x if x is invertable or if the computation has already been performed: returns the cached inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
