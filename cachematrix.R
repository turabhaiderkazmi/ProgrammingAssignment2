## The R functions in this script take a square matrix, cache its data
## and its inverse in a seperate function.

## The 'makeCacheMatrix' function takes a square matrix as an input
## and creates a list can cache the inverse of the input matrix into a 
## special 'matrix' (class=list). 
## We set and get value of the matrix and its inverse in this function.

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
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


## The 'cacheSolve' function calculates the inverse of the special matrix
## created in the 'makeCacheMatrix' function. It checks if the inverse is 
## already calculated or not. If it is, the function will return the already
## calculated inverse otherwise it will use Solve() function to calculate the 
## inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
      if(!is.null(m)) {
             message("getting cached data")
             return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
