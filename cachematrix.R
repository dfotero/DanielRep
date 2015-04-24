## I created two functions that save in cache the inverse of a matrix to use it
## again in a future so it soes not need to be calculated again.


## The function makeCacheMatrix creates a list with the following functions:
## set: which sets the matrix into a variable
## get: which gets the matrix
## setinv: which sets the value of the inverse of the matrix
## getinv: which gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the function cacheSolve receives as parameter the list the was create 
## with the function makeCacheMatrix.
## In cacheSolve first it looks at the function getinv() and see if the inverse
## of the matrix was already calculated. If not then it uses the solve() function
## to calculate the inverse of the matrix and store it in cache using the function
## setinv().

cacheSolve <- function(x, ...) 
{
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
