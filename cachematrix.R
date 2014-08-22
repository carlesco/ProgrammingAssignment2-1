## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # inversa will store the cached inverse matrix
        # it's reset to NULL every time makeCacheMatrix is called
        inversa <- NULL
        
        # this function stores the value to the matrix in the object
        # and reset to NULL the internal matrix
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        # get method for the matrix
        get <- function() x
        
        # this is called by cachesolve() during the first cachesolve()
        # access and it will store the value using superassignment
        setinv <- function(finv) inversa <<- finv
        
        # this will store the cached value on next accesses
        getinv <- function() inversa
        
        # this list is returned with the newly created object
        # It lists all the "methods" that are part of the object
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inversa <- x$getinv()
        
        # Check if the inverse has been already calculated, if so return the value
        if (!is.null(inversa)) {
                message("Dades extretes de la catxé")
                return(inversa)
        }
        
        # if reached this point, the inversa has not been calculated. call Solve!
        data <- x$get()
        inversa <- solve(data, ...)
        
        # Store the inverse for future use
        x$setinv(inversa)
        inversa
}
