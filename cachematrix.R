## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. These functions will create a special matrix that can cache its
## inverse and retreive it from cache if it has been cached.

## First, pass a matrix into makeCacheMatrix to make the cache matrix. Then,
## the inverse is stored and calculated on the first run of 
## cacheSolve on the new matrix. Finally, retreive the cache data on subsequent
## calls of cacheSolve.


## Example Usage:
## myMatrix <- matrix(1:4, 2, 2) 
## myCacheMatrix <- makeCacheMatrix(myMatrix)
## cacheSolve(myCacheMatrix)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## cacheSolve(myCacheMatrix)
##Getting cached data...
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5



## makeCacheMatrix - creates a special ""matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve - This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should retreive
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}





