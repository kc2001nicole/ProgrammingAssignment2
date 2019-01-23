## These functions solve for the inverse of a matrix. 
## If the inverse of the matrix has been solved previously, it returns the Cached version.

## makeCacheMatrix creates a special "matrix" or list containing the set value of the matrix, the get 
## value of the matrix along with the set and get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

invMat <- NULL
setMatrix <- function(y) {
	x <<- y
	invMat <<- NULL
	}

getMatrix <- function() x
setinverse <- function(inverse) invMat <<- inverse
getinverse <- function() invMat
list(setMatrix =setMatrix, getMatrix = getMatrix,
     setinverse = setinverse, getinverse = getinverse)

}


## Calculates the inverse of the matrix created with the above function unless the inverse has
## already been calculated, The it returns the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invMat <- x$getinverse()
	if (!is.null(invMat)){
	message("getting cached inverse matrix")
	return(invMat)
	}

data <- x$getMatrix()
invMat <- solve(data, ...)
x$setinverse(invMat)
invMat


}
