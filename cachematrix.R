## speeds up computation of a matrix inverse by cacheing its value
## starting with a square matrix m, you would enter the following:
## n <- makeCacheMatrix(m)
## cacheSolve(n)


## Makes a list of four functions that set and get the value of
## the matrix, and set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## checks to see if the matrix has a cached inverse already computed.
## If not, it solves for it and stores it in cache within the list
## created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("the inverse of your matrix:")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinv(inv)
    inv
}
