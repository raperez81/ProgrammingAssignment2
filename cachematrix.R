## Due to Matrix inversion is usually a costly computation, the next functions manage a matrix object
## computing the inverse and saving it in cache reducing the time spent to perform its inverse

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL ## when "x" changes, its inverse is set to NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  library(MASS)
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- ginv(data)
  x$setinverse(i)
  i
}


