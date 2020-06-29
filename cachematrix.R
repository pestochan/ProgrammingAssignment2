## Compute the inverse of a matrix if the inverse does not exist in the cache yet.

## Function 1
# create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
                
}


## Function 2
# see if inverse of the special matrix already exist. 
# If so, retrieve the inverse;
# If not, compute the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        a <- x$get()
        inv <- solve(a, ...)
        x$setinverse(inv)
        inv
}