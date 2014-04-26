#R Programming Course - Programming Assignment 2 - Peer Assessment

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
#via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached Inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
