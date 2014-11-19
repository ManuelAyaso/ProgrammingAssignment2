## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function which returns a list
## with methods to get and set the matrix and to
## get and set the inverse of that matrix in case that it
## has been already cached
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This function will solve the inverse of the matrix,
## trying first to get the inverse if it has been cached,
## or calculating the inverse if it hasn't and then
## it will be cached before returning the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
