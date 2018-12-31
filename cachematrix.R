
## Goal of this assignment is to write functions on "makeCacheMatrix"
## and "cacheSolve"


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
get <- function () x
setinvr <- function(inverse) inv <<- inverse 
getinvr <- function() inv
list(set = set, get = get, setinvr = setinvr, getinvr = getinvr)
}

## cacheSolve returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
if(!is.null(inv)){
  message("getting cached result")
  return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinvr(inv)
inv
}
