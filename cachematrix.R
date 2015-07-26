## These Functions get the inverse of a matrix.
## Computing of matrix inversion usually consumes so much time that
## once computed the inverse of a matrix, the functions cache it 
## and get it from the cache until the matrix is changed.

## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
  
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        
        list(set = set,
             get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## (If the inverse has already been calculated (and the matrix has not changed), 
##  then the function should retrieve the inverse from the cache.)
cacheSolve <- function(x, ...) {
        ## Get cached data
        s <- x$getSolve()
        
        ## If cached data exists, return it.
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        s <- solve(data)
        x$setSolve(s)
        s
}
