## These fucntions calculate the inverse of a matrix and cache the value to be retrieved later to avoid having to 
## recalculate the values

## The makeCacheMatrix function creates a list of functions to:
## setMatrix: Set the matrix for which the inverse matrix will be calculated
## getMatrix: Retrieve the value of the set matrix
## setInverse: Set the value of the inverse matrix for the set matrix
## getInverse: Retrieve the value of the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) { ##The setMatrix function would be used to reassign the matrix in an object 
                x <<- y            ##set the solved inverse matrix value to null so the a recalculation would
                m <<- NULL         ##be triggered when cachSolve() is called again for that object
        }
        getMatrix <- function() x
        setInverse <- function(SolvedMatrix) m <<- SolvedMatrix
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}
  

## The cacheSolve function calculates the inverse of the set matrix in x, unless the inverse has already been calculated
## If already calculated, the value of the cached inverse matrix is retrieved

cacheSolve <- function(x) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached inverse matrix")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data)
        x$setInverse(m)
        m ##Return a matrix that is the inverse of the set matrix in x
}
