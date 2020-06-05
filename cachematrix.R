##  These functions cache the inverse of a matrix

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y) {
                x <<- y
                inv <<- matrix()
        }
        get <- function() x
        setinv <- function(inversion) inv <<- inversion
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!as.logical(is.na(inv))) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv        
}
