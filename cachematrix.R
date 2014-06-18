## These two functions allow caching result of matrix inverstion
## This particulary helps when dealing with huge matrix 

## the function makeCacheMatrix is the constructor for our cached matrix
## it implements getter and setter for the matrix x 
## and for its inverse s

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## the cacheSolve function returns the inverse of CacheMatrix object as parameter
## if this object already has the inverse calculated (so s will not be null)
## we simply return it;  either we process the inverse calculation, set
## the value of s with the inverse, and finally return it

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
