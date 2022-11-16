## hilbert function makes a square matrix, taking size of matrix. If hilbert(2) 
## entered, then make 2 by 2 square matrix. when 5 entered, then make 5 by 5 matrix.
## makeCahceMatrix is to store matrix and return matrix
## cacheSolve check there is inverse matrix is in there already or not. if it has, then 
## return it. if it does not have it, then make a inverse matrix by using solve function

## hilbert function makes a square matrix, taking size of matrix. If hilbert(2) 
## entered, then make 2 by 2 square matrix. when 5 entered, then make 5 by 5 matrix.
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, `+`) }



## makeCahceMatrix is to store matrix and return matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}
x <- hilbert(2)

y <-matrix(rnorm(36),6,6)

## cacheSolve check there is inverse matrix is in there already or not. if it has, then 
## return it. if it does not have it, then make a inverse matrix by using solve function
cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
## checking both functions are working right.
cacheSolve(makeCacheMatrix(x))
y <- makeCacheMatrix(x)
cacheSolve(y)
cacheSolve(y)
y
x
