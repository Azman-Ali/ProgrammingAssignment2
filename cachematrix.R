## This files comprises of makeCacheMAtrix and cacheSolve Function
## Both files follows similar code structure of the code examples from the instruction 
## Note that this functions assumes that all matrix supplied are invertible 

## This function create a special matrix object that cache its inverse
## It basically contains list of function to
## 1. Set the value of matrix
## 2. Get the value of matrix
## 3. set inversed value of matrix
## 4. Get the inversed value of matrix
## (The inverse matrix calculation is done using solve() function in R)

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
