## This files comprises of makeCacheMAtrix and cacheSolve Function

## This function create a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    m<-NULL
    set <- function(y) {
	x <<- y
        m <<- NULL
    }

    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set = set, 
	 get = get,
	 setMatrix = setMatrix,
	 getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## The inverse matrix calculation is done using solve() function in R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       m<- x$getMatrix()
       if(!is.null(m)){
           message("Getting Cached Data")
	   return(m)
	}

	data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
	m

}
