## Put comments here that give an overall description of what your
## functions do
## the two functions are used to create a special object that 
## stores a matrix and cache's its inverse.

## Write a short comment describing this function
# set a list containing functions to 
#  1) set value of matrix
#  2) get value of matrix
#  3) set inverse of the matrix
#  4) get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
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
## from input of list created by makeCacheMatrix,
## get the cache of inverted matrix, if exists, else, 
## calculate inverse and set result to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

# test case
# A = makeCacheMatrix(matrix(c(-3,5,1,0),2,2))
# cacheSolve(A)