# ===============================================================================================
# ===============================================================================================
# ===============================================================================================

# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# It contains a list of functions as follow:
# set the value of the input matrix
# get the value of the input matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matrix
    inv <- NULL

    # set matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL   
    }
    
    # get matrix value 
    get <- function() x
    # set inverse matrix
    setinv <- function(inv_) inv <<- inv_
    # get inverse matrix
    getinv <- function() inv

    # list of all functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


# ===============================================================================================
# ===============================================================================================
# ===============================================================================================

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # if the inverse is already exist then cache
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    # Calculate inverse matrix using solve function
    inv <- solve(data, ...)
    
    # cache that inverse matrix
    x$setinv(inv)
    
    # return 
    inv
}

# ===============================================================================================
# ===============================================================================================
# ===============================================================================================

