## makeCacheMatrix function act as a container for the input matrix 
## as well as it's inverse
## cashSolve funcation takes a matrix container makeCacheMatrix instance
## and returns its inverse.


## This is the matrix container function which provider get and set 
## operations for the input matrix as well as it's inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes an input makeCacheMatrix instance
## returns its inverse.
## if the inverse has been calculated before, cacheSolve return the inverse directly
## otherwise, it calculates the inverse and update the container, then return the inverse
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
                message("getting cached data")
                ## return cached inverse
                return(m)
        }
        
        ## calculate inverse
        message("calculating data")
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m  
}