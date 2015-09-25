## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(M = matrix()) {
    M_inv <- NULL
    set <- function(A) {
      M <<- A
      M_inv <<- NULL
    }
    get <- function() M
    set_inverse <- function(inverse) M_inv <<- inverse
    get_inverse <- function() M_inv
    
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(M, ...) {
    ## Return a matrix that is the inverse of 'M'
	M_inv <- M$get_inverse()
	# use cached inverse of the matrix
	if(!is.null(M_inv)) {
		message("getting cached inverse of the matrix")
		return(M_inv)
	}
    
    # calculate inverse matrix
    M_inv <- solve(M$get())
    # cache it
    M$set_inverse(M_inv)
    return(M_inv)  
}




