##  Two functions in this file will cache the inverse of a matrix.


## The makeCachematrix function creates a "matrix", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix
## get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse_matrix) m <<- inverse_matrix
  get_inverse<- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## ## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated, 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
