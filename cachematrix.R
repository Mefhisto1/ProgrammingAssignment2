## Put comments here that give an overall description of what your
## functions do

## This functions creates a special matrix object and computes the inverse 
## of the given matrix, and can also fetch and set the original matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Sets the matrix and assigns inverse matrix to null
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # returns the matrix
  get <- function() x
  
  # returns the inverted matrix
  getInverse <- function() inverse
  
  # inverts the matrix and caches it in variable 'inverse
  setInverse <- function() {
    inverse <<- solve(x)
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function gets the inverted matrix from cache
## If there's no inverted matrix in the cache, it inverts the given matrix
## And returns that as a result

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  
  # Get the matrix from makeCacheMatrix
  
  invertedMatrix <- x[["getInverse"]]()
  
  # If the matrix is already inverted, return that matrix
  # Otherwise, invert the matrix and return the result
  if (!is.null(invertedMatrix)) {
    message('getting the cached inverted matrix')
    return(invertedMatrix)
  } else {
    return (solve(x))
  }
  
}
