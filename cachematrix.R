## Functions are used to cache inversion of matrix operation
## instead of realculation for identical matrix


## Function constructs a "matrix wrapper" as list of functions 
## for accessing and setting underlying data.
## 'x' is a matrix to be inverted
## Return list of functions:
# get() - returns original matrix
# set(y) - replaces original matrinx with new one;
#          clears cached value of previous inversion
# setInverse(inverted) - saves inverted matrix to cache
# getInverse() - returns inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    
    cachedInv <- NULL
    
    get <- function() x
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }
    setInverse <- function(inverted) cachedInv <<- inverted
    getInverse <- function() cachedInv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function solves matrix wrapped by makeCacheMatrix() if there is no cached
## value and saves result to cache. Otherwise return cached result.
## 'x' - "matrix wrapper" returned by makeCacheMatrix().
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    solvedMatrix <- x$getInverse()
    if(!is.null(solvedMatrix)) {
        message("getting cached data")
        return(solvedMatrix)
    }
    originalMatrix <- x$get()
    solvedMatrix <- solve(originalMatrix, ...)
    x$setInverse(solvedMatrix)
    solvedMatrix
}
