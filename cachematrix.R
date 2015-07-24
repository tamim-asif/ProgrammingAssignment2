## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## set function sets a matrix and get function returns the matrix to the caller.
## setinverse function set the inverse of a matrix if already computed.
## getinverse function returns if there is already any cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(mat){
          x <<- mat
          m <<- NULL
        }
  
        get <- function() x
  
        setinverse <- function(inverse) m <<- inverse
  
        getinverse <- function() m
  
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}


## Write a short comment describing this function
## This function checks whether a given matrix is already inversed by checking
## through the getinverse function of the previous function.
## If no cached matrix is found then it inverse the matrix using solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
          message("getting cached inverse matrix")
          return(m)
        }
        
        inverse <- x$get()
        m <- solve(inverse)
        x$setinverse(m)
        m
}
