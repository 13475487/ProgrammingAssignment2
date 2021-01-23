## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Well I guess it's pretty simple - it makes a matrix 
#that can cache the inverse function.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverse <<- inverse
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function

#This just checks if the inverse has been already done and returns
#a message "getting cached data" as well as the inverse.
#However if it has not been calculated it calculates the inverse and returns
#the result in the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}