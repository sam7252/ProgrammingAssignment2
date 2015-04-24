## Together the two functions compute the inverse of a matrix and cache the
## inverse. If the matrix has not been changed, rather than recalculate the
## inverse each time, it may be retrieved from the cache.

## Takes a matrix x as an argument and returns a special "matrix" of four
## functions that set matrix x, return matrix x, set the inverse of x and
## return the inverse of x

makeCacheMatrix <- function(x = matrix()) {

        invx <- NULL # creates a placeholder for the inverse of matrix x and
        # sets it to NULL
        
        ## Defines function to change matrix x to an inputted matrix y
        ## and reset the inverse of matrix x to NULL
        setmatx <- function(y = matrix()) {
                x <<- y
                invx <<- NULL
        }

        ## Defines a function to return matrix x
        getmatx <- function() x

        ## Defines a function to cache the inverse of matrix x to an inputted
        ## argument (calculated by the cacheSolve function)
        setinvx <- function(inverse) invx <<- inverse
        
        ## Defines a function to return the inverse of matrix x
        getinvx <- function() invx
        
        ## Returns a special "matrix" containing the functions defined above
        ## and the environments in which each is defined
        list(setmatx = setmatx, getmatx = getmatx,
             setinvx = setinvx,
             getinvx = getinvx)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves it from the cache.

cacheSolve <- function(x, ...) {
        invx <- x$getinvx() # Gets the cached value of the inverse of matrix x
        
        ## Ifcached value is NOT null (by default matrix x has NOT changed,
        ## as the inverse is reset to NULL for every new matrix x inputted by
        ## the makeCacheMatrix or setmatx functions), returns the cached value
        if(!is.null(invx)) { 
                message("getting cached data")
                return(invx)
        }
        
        ## If cached value is null, calculates, caches & returns the inverse
        ## of matrix x
        data <- x$getmatx() # stores matrix x (inputted as an argument into
        # makeCacheMatrix or setmatx functions)
        invx <- solve(data, ...) # calculates the inverse of matrix x
        x$setinvx(invx) # caches the inverse of matrix x
        invx # returns the inverse of matrix x
}
