## The first function, makeCacheMatrix creates a special "matrix" to 
# set the matrix
# get the matrix
# set the inverse matrix 
# get the inverse matrix

## is the same as the example but use a matrix and the 
#function solve (which returns the inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
#the cache and skips the computation. Otherwise,
# it calculates the inverse of the data and sets the value of the mean in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}    ## Return a matrix that is the inverse of 'x'

