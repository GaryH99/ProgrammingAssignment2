## Since inverting a matrix can be time consuming as a program runs,
##      we want to create a way to use a cached version of the inverted matrix
##      if the matrix has not changed since the last time the inversion was needed

## makeCacheMatrix(x)
##      x is matrix to be inverted
##      this function creates the following functions
##      set -- sets the matrix
##      get -- gets the matrix
##      setinv -- sets inverse of matrix (direct calls to this are discouraged)
##      getinv -- gets inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve
##  input -- matrix to be inverted
##  output -- inverted matrix  (NULL if no inverted matrix available)
##
##  returns cached inversion if available or computes inversion if necessary
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
