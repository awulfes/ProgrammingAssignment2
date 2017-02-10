## The following two functions are my solution to the 2nd programming assignment in the coursera R programming course from JHU.
## The objective is to two create a function that can cache computet results.

## Example usage:
## a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## cacheSolve(a)
## returns:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(a)
## second call returns:
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

# Creates a matrix object with the ability to cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Returns a matrix that is the inverse of 'x' using a cache
cacheSolve <- function(x, ...) {
  
  # Retrieve cached inverse matrix
  m <- x$getInv()
  
  # If not null then return cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If data not in cache get matrix and solve/inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # Put inverse matrix to cache
  x$setInv(m)
  
  # return solved matrix
  m
}