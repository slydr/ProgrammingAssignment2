## Put comments here that give an overall description of what your
## functions do

## Declare and define function named makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL             ## assign NULL to variable 'm'
  set <- function(y){
      x <<- y       ## define x
      m <<- NULL
  }
  get <- function() x    ## create function get()
  setmatrix <- function(matrix) m <<- solve  ## create function setmatrix()
  getmatrix <- function() m                 ## create function getmatrix()
  list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)  ## define output list of function
}


## Declare and define function named cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){              ## if condition check to see if m has value; if so, output message and don't recalculate 
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()             ## otherwise, solve for m (matrix) which is the inverse of 'x'
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m                            ## return m
}
