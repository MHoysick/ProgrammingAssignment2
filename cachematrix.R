## makeCacheMatrix takes a matrix and its inverse and caches them to be recalled later
## cacheSolve checks to see if the inverse of the matrix has already been calculated
## if not, it calculates the inverse which is then cached

## creates an empty matrix i where it will store the inverse of x
## It then defines and stores 4 functions to cache and return the inverse of x
## setNew updates the original matrix to a new matrix y and resets the inverse matrix i
## get returns the original matrix
## setInverse assigns the inverse of matrix x to i
## getInverse returns the inverse of matrix x
## the functions are listed so that we can recall them later


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	setNew <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	getMatrix <- function() x
	
	setInverse <- function(inverse) i <<- inverse
	
	getInverse <- function() i

	list(setI = setI, get = get, setInverse = setInverse, getInverse = getInverse)
}


## first uses getInverse() to recall the value of i from makeCacheMatrix()
## It checks if the inverse has already been calculated and if so returns the cached value of i
## If i is null, meaning the inverse hs not been calculated yet, cacheSolve pulls the matrix
## using getMatrix() and assigns its inverse in makeCacheMatrix() using setInverse()


cacheSolve <- function(x, ...) {
        i <- x$getInverse()
	
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}

	orgMatrix <- x$getMatrix()
	i <- solve(imatrix)
	x$setInverse(i)
	i

        ## Return a matrix that is the inverse of 'x'
}
