## The purose f this code is to store a matrix and use that stored
##matrix to compute and output the inverted matrix




## the MakeCacheMatrix function takes a matrix as input and establishes the global variables
## needed to compute the inverted matrix. it outputs a list contianing functions that are used 
## to quickly output the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## the CacheSolve function takes the output from makeCacheMatrix and determines where (in memory) to find
## the needed matrix and then using the Solve function outputs the inverted matrix.
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
