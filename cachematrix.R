## The functions performs the following:
## makeCacheMatrix creates a special "matrix" object 
##                 that can cache its inverse
## cacheSolve efficiently computes the inverse of 
##            the special "matrix" using cached data

## makeCacheMatrix creates a list of functions to:
## 1: set the matrix
## 2: get the matrix
## 3: set (cache) the inverse of matrix
## 4: get previously cached inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {

  ## cache variable to store the inverse matrix
  invX <- NULL
  
  ## function to set new matrix
  setMatrix <- function(newM) {
    x <<- newM
    ## if th new matrix was stored,
    ## the cache should be cleaned
    invX <<- NULL
  }
  
  ## function to get stored matrix
  getMatrix <- function() x
  
  ## function to set calculated inverse matrix
  setInv <- function(inv) invX <<- inv
  
  ## function to get cached inverse matrix
  getInv <- function() invX
  
  ## all functions stored in list
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix,
    setInv    = setInv,
    getInv    = getInv
  )
}


## cacheSolve computes the inverse of the special
## "matrix" returned by makeCacheMatrix above;
## if the inverse has already been calculated 
## and the matrix has not changed, then the cached 
## value should be retrieved

cacheSolve <- function(x, ...) {
  ## try to get cached inverse matrix
  inv <- x$getInv()
  if (!is.null(inv))
    return(inv)
  
  ## if cache is null, calculate inverse ...
  data <- x$getMatrix()
  inv <- solve(data, ...)
  ## ... cache and return result
  x$setInv(inv)
  inv
}
