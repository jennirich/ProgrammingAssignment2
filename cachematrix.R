## Create a matrix object that can cache its inverse, then make a
## function that can calculate and store the inverse in the cache or retieve the
##inverse, if it has already been stored; storage happens in the parent environment.

## functions 
## 1. set the value of the matrix; 
## 2. get the value of the matrix; 
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


## Set and get the value of the matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function () i
    
    ##this is a function list where functions get named
    list(set = set, get = get, 
        setsolve = setsolve,
        getsolve = getsolve)
}


## Calculate the inverse of the matrix, after checking to see if the matrix inverse has been cached.
## If has been cached, return cached value, otherwise
## return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    i <- x$getsolve()
    if(!is.null(i)) {
            message("getting cached data")
            return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}

##checking to see if functions work by first creating the matrix object to hold the inverse matrix, then using the
##cacheSolve function to either return the cached data or calculate the inverse and return that

a <- makeCacheMatrix(matrix(c(0,5,8,7,5,5,3,4,9),3,3))

b <- cacheSolve(a)

b

b <- cacheSolve(a)

b





