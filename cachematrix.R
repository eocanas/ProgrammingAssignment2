
## MakeCacheMatrix creates a list of functions that sets and gets the values around
## an inversible matrix. cacheSolve is a function capable of extracting some of the values
## process them and set them in said list.

## Takes an invertible matrix as an input and generates a list that 
## contains the internal functions (get, set, setinverse, getinverse) 
## to be accessed later by other functions using x$get, x$set, x$setinverse, 
## x$getinverse. This functions uses solve() to invert the matrix. Vectors result 
## and x are sent beyond their scopes so they can be reached in a diverse environment.

makeCacheMatrix <- function(x = matrix()) {
        result <- NULL
        set <- function(y) {
                x <<- y
                result <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) result <<- solve
        getInverse <- function() result
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## Takes the makeCacheMatrix list as an input, if result is present returns the result
## if it is not present uses x$get to generate the result using solve, and sets the value
## using x$setInverse. Presents the result.

cacheSolve <- function(x, ...) {
        result <- x$getInverse()
        if(!is.null(result)) {
                message("getting cached data")
                return(result)
        }
        data <- x$get()
        result <- solve(data, ...)
        x$setInverse(result)
        result
}
