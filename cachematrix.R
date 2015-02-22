## Programming Assignment 2 
## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

##The first function, makeMatrix creates a special "Matrix",
##which is really a list containing a function to
	##set the value of the Matrix
	##get the value of the Matrix
	##set the value of the Inverse
	##get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) { 
## Create a Martix object (x) and some associated subfunctions or methods
	## Define Cache (m)
 	m <- NULL
   	set <- function(y) {
         		x<<- y    ## Assign input matrix y to variable x in the parent environment
         		m <<- NULL ## reintializing parent environment to NULL
		}
	get <-function() x ## this returns matrix x
	setinverse <- function(inverse) m <<- inverse ## set the cache(m) equal to the inverse of Matrix (x)
	getinverse <- function(inverse) m ## Returns cache inverse of (x)

	list(set = set, get = get,
		setinverse = setinverse,
      	getinverse = getinverse)		

}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the
##matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) { 
##Return the matrix that is inverse(x)
	m <- x$getinverse()
	if(!is.null(m)) {
                message("getting cached data")
                return(m)
		}
	data <-x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

## Sample run:
## > x = rbind(c(2, 3), c(4, 5))
## > m = makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
##[1,]    2    3
##[2,]    4    5

## No cache first round
## > CacheSolve(m)
##[,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0

##> cachSolve(m)
##getting cached data in the second run
##     [,1] [,2]
##[1,] -2.5  1.5
##[2,]  2.0 -1.0


