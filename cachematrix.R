## example on how to use this functions
## pass a square invertible matrix as the
## argument to makeCacheMatrix. save the
## return value of makeCachMatrix and pass that
## as the argument to cacheSolve.
## makeCacheMatrix returns a list of 4 functions that
## are used to cache the orginal matrix in its own
## enviroment. When cachSolve runs with this argument
## it solves for the inverse of the matrix passed to 
## makeCacheMatrix and caches the answer m using x$setinv(m)
## The follwing was copy and pasted from the console.
##
## Example of how it works
##
## Create matrix in console and store it in matrx1
## Console Paste below
## ****************
## > matrx1 <- matrix(c(2,3,1,4), nrow=2, ncol=2)
## > matrx1
##      [,1] [,2]
## [1,]    2    1
## [2,]    3    4
## > 
## ****************
## now call makeCacheMatrix(matrx1) as follows:
## *****************
## > mc1 <- makeCacheMatrix(matrx1)
## *****************
## Now call cachesolve(mc1) to solve for the inverse
## of matrx1, as follows:
## mc1inv <- cacheSolve(mc1)
## ************
## > mc1inv <- cacheSolve(mc1)
## > mc1inv
##      [,1] [,2]
## [1,]  0.8 -0.2
## [2,] -0.6  0.4
## > mc1inv <- cacheSolve(mc1)
## getting cached data
##> mc1inv
##      [,1] [,2]
## [1,]  0.8 -0.2
## [2,] -0.6  0.4
## >
## *****************
## additional calls to cacheSolve will not recalculate
## the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
