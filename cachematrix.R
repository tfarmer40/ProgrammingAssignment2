## Creates two functions in order to store matrix inverse in cache.
## Function 1 creates the cache system
## Function 2 looks for an inverse in cache and calculates it if needed.
## Upon calculation of inverse it stores it in the cache.

## Creates object with initial matrix, null inverse, and setters & getters

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse as null.
  i <- NULL
  ## Source a function in case user inputs new matrix, reset inverse to null.
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  ## Source a function to return the most recently input matrix.
  get <- function() x
  ## Source a function to cache matrix inverse
  setinv <- function(inv) i <<- inv
  ## Source a function to retrieve the cached inverse
  getinv <- function() i
  ## Enable each function output to be retrieved using $
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve looks for the inverse of this matrix in the cache.
## If it is there, it retrieves it from the cache.
## If not, it uses the Solve function to get the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Start with seeing if an inverse is in the cache
  i <- x$getinv()
  ## If in the cache, return what is there, with a message.
  if(!is.null(i)){
    message("getting from cached data")
    return(i)
  }
  ## If not in the cache, use solve function to calculate inverse.
  ## First, retrieve the matrix from the cache
  data <- x$get()
  ## Then use solve function on that matrix
  i <- solve(data,...)
  ## Lasty, save inverse in the cache
  x$setinv(i)
  ## Last output is function ouput, so print inverse.
  i
}