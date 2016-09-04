## Put comments here that give an overall description of what your
## functions do
## Calculate the inverse of a matrix and save to the cache so it can be 
## retrived if it hasn't changed. 
## This saves calculating it every time.

## Write a short comment describing this function
## Create a "special matrix" to enable us to cache its inverse.
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


## Write a short comment describing this function
## Check if x already has an inverse saved.
## if not then we calculate the inverse.
## If it does then we can just get that value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
