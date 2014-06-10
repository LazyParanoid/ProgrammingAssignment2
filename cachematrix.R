## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly .
## The two functions defined here basically want to cache and the inverse
## matrix when it is caculated, and reuse the cached inverse if needed later .

## makeCacheMatrix creates a speacial "matrix" object, 
## which is really a list of 4 functions containing:
## 1)set(A), set the matrix  of this object to be matrix A
## 2)get(), get the matrix of this object
## 3)getinverse(), get the cached inverse matrix
## 4)setinverse(A), cache the inverse matrix to be matrix A

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solution) inverse <<- solution
    getinverse <- function() inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)    
}


## cacheSolve(x)
## directly get the cached inverse matrix of x if it has been calculated.
## Otherwise, solve for the inverse matrix and cache it in object x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    result <- x$getinverse()
    if (!is.null(result)) {
        message("getting cached inverse matrix")
        return(result)
    }
  
    mat <- x$get()
    sol <- solve(mat)
    x$setinverse(sol)
    sol
}
