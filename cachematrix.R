## This program does two things:
## 1) Computes inverse of a matrix and
## 2) Caches results of the inverse computation
## 
## Usage: 
## > b = makeCacheMatrix(a) ## where a is a inversible square matrix
## > cacheSolve(b)

## makeCacheMatrix function takes a matrix as input and 
## returns an object to be passed on to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrixInverse <<- inverse
    getinverse <- function() matrixInverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function takes the output of makeCacheMatrix as input and 
## returns the inverse of the matrix passed to makeCacheMatrix

cacheSolve <- function(x, ...) {
    matrixInverse <- x$getinverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    data <- x$get()
    matrixInverse <- solve(data, ...)
    x$setinverse(matrixInverse)
    matrixInverse
}
