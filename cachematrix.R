## makeCacheMatrix takes a matrix as an input.  Then user sets a new variable as the output
## of makeCacheMatrix, and uses the new variable as the input for cacheSolve to return the 
## inverse matrix of the original
## e.g.:
##  >    makeCacheMatrix (matrixj)
##  >    matrixj1 <- makeCacheMatrix (matrixj)
##  >    cacheSolve (matrixj1)
##    Returns inverse of matrix - by calculating it - And stores the inverse in cache.
##    Running cacheSolve again will return the same inverse, but will return it from cache
##    The 2nd and subsequent returns of the same matrix will state "getting cached data"



makeCacheMatrix <- function(x = matrix()) {
  ## declare x to be a matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  m <- NULL  ## set m as a marker - initial value of null indicates cache is empty.
  set <- function(y = matrix) {
    x <<- y
    m <<- NULL
    ## <<- sets x and m values in parent environment so they can be used by other functions.
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve  ## sets m in parent environment so if matrix has already 
  ## been solved, this function makeCacheMatrix won't reset m to null for the given matrix.
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## cacheSolve takes matrix w/ other output from makeCacheMatrix function. Then
## checks to see if the matrix inverse is in cache (by checking if m is null or not).
## Returns the inverse value of x - 1st by calculating if m is null, then by returning
## the data from cache if m is not null.

cacheSolve <- function(x = matrix(), ...) {
     m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}


