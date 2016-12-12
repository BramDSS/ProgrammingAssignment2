## There are two functions:
## 1. makeCacheMatrix: which creates an object with a matrix (the input),
##      the inverse of the matrix and fuctions to retrieve and/or set these
##      matrices
## 2. cacheSolve: if the inverse of the matrix (part of input-object) has
##      already been calculated, this inverse is retrieved, else the inverse is
##      calculated and stored in the input-object.

## The following set of two functions create a possibility to avoid time
## consuming calculation of repeatingly calculating the same inverse matrix.
## The inverse matrix can be calculated w=once and stored in memory.



## 'makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## Both the (input) matrix and the inverse matrix (if computed) are stored in 
## memory. Four fuctions ar added, to "set" the matrix and the inverse matrix
## and to retrieve both the matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv_mat <<- inv
    getinv <- function() inv_mat
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## `cacheSolve`: This function computes the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has already been
## calculated (and the matrix has not changed), then `cacheSolve` should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getinv()
    if(!is.null(inv_mat)) {
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data)
    x$setinv(inv_mat)
    inv_mat
}
