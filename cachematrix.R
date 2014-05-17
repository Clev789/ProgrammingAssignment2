## The base for this function was taken from Programming Assignment - calculating mean of vector
## The function stores the value of matrix inverse in cache
## to save time on costly computations
## If inverse of the function was not stored before - it will be computed

## The function makeCacheMatrix creates a class with 4 methods (internal 
## functions) - create matrix,
## get matrix, calculate inverse and get inverse; Later those methods will be
## for calculating inverse

makeCacheMatrix <- function(x = matrix()) {
  # Set to null functions will not be invoked (e. g. a <- makeCacheMatrix())  
  m <- NULL

  # Set matrix value  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Get matrix value  
  get <- function() x

  # Calculate matrix inverse
  setinverse <- function(solve) m <<- solve

  # Receive matrix inverse (previously calculated)
  getinverse <- function() m
  
  # assigning names to the methods / functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function operates with the object created by makeCacheMatrix
## it can either retrieve the value for already calculated inverse or
## calculate inverse value and assign it to the object

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x' from object created by 
  ## makeCacheMatrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the the inverse was not calculated before - calculate it
  data <- x$get()
  m <- solve(data, ...)
  
  ## Assign the inverse back to the object
  x$setinverse(m)
  m
}
