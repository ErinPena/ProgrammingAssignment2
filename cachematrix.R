## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to set and get values of the matrix and 
## then set and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()){
  Inverse <- NULL
  set <- function(y){
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  SetInverse <- function(InverseMatrix) Inverse <<- InverseMatrix
  GetInverse <- function() Inverse
  list(set = set, get = get,SetInverse = SetInverse,GetInverse = GetInverse)
}


## This function calculates the inverse of the "matrix" 
## made with the above function. First seeing if the 
## inverse has already been calculated. Iff, it gets the inverse from the 
## cache and avoids doing the computation again. Otherwise, it calculates the inverse from the setinv 
## function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inverse <- x$GetInverse()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  myMatrix <- x$get()
  Inverse <- solve(myMatrix, ...)
  x$SetInverse(Inverse)
  Inverse
}