## Functions to store inverse of input matrix into cache and revert back the 
## data from cache 

## Creates 4 functions get, set, setinv and getinv
## Variables x and i are assigned values in the parent env.
## The 4 functions and above variables are limited to the environment of the
## created object using makeCacheMatric function
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
        
}


## Checks if data exists in cache then fetches and returns it else
## inverts the matrix using 'solve' function, updates cache and returns the
## inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}


## Use below commands to test the functionality
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## x$get()
## inv <- cacheSolve(x)
## inv
## inv <- cacheSolve(x)
## inv
## x$get() %*% x$getinv()
## above command should return a identity matrix
