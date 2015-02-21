## Calculates the inverse of a matrix, with cached results.

## Progamming Assignment 2, Rodrigo Medeiros.

## Create the special type of Matrix what is similar to attributes and methods
makeCacheMatrix <- function(mtrx = matrix()) {
  minv <- NULL
  set <- function(y) {
    mtrx <<- y
    minv <<- NULL ## Set inverse to null as it was not calculated yet
  }
  get <- function() mtrx
  setinv <- function(solve) minv <<- solve(mtrx) ## calculate inverse
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculate the inverse of the matrix and cache results
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    imr <- x$getinv()  ## imr = Inverted Matrix Result
    if(!is.null(imr)) {
      message("getting cached data")
      return(imr)
    }
    data <- x$get()
    imr <- solve(data)
    x$setinv(imr)
    imr ## Return inverted matrix
}
