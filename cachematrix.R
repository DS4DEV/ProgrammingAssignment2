## Two functions in pair caching the inverse of a matrix
## Datascience Coursera - RProgramming - Week 3 Assignment

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inverse <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Method the get the matrix 
  get <- function() x
  
  ## Method to get the inverse of the matrix
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  
  ## Return a list of the methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Computes the inverse of the matrix returned by "makeCacheMatrix"
## The "cachesolve" should retrieve the inverse from the cache, if the inverse has already been calculated

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'  
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}
        