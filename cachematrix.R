## Create function makeCacheMatrix  for Cache Matrix
## Create function cacheSolve  for returns Matrix Inverse
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y) {
        x <<- y
        inve <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inve <<- inverse
    getinverse <- function() inve
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix but before review exists in cache memory 

cacheSolve <- function(x, ...) {
    inve <- x$getinverse()
    if(!is.null(inve)) {
        message("getting cached data.")
        return(inve)
    }
    data <- x$get()
    inve <- solve(data)
    x$setinverse(inve)
    inve
}
