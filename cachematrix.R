## A pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix of x
  ix <- NULL
  
  # function to set the matrix
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  
  # function to get the matrix
  get <- function() x
  
  # function to set the inverse
  setinverse <- function(inverse) ix <<- inverse
  
  # function to set the inverse
  getinverse <- function() ix
  
  ## Return a lost of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  
  ## if set, return inverse
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse with solve()
  ix <- solve(data)
  
  ## set the inverse
  x$setinverse(ix)
  
  ## return the inverse matrix
  ix      
}
