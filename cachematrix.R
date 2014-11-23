## There are 2 function in this programming asisgnment
## First function makes a list  with methods that sets and gets a matrix
## and its inverse 
## The second function is passed the list from the first and validates
## if the inverse is already calculated or not. If not calculated, it will calculate 
## and return back.


makeCacheMatrix <- function(x = matrix()) {

# Initialize the Inverse 
  
  xInverseCache <- Null
  
  set <- function (x=matrix()) {
    x <<- matrix
    xInverseCache <<- Null
  }

  get <- function() x

## Method to set the inverse of the matrix
   setInverse <- function(inverse) { xInverseCache <<- inverse}
  
## Method to get the inverse of the matrix
getInverse <- function() xInverseCache 
  
## Return a list of the methods
list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)

}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the Inverse has already been calculated of this matrix.
## then return from Cache otherwise calculate , save and return.

cacheSolve <- function(x,...) {
  
  ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse
  
## Just return the inverse if its already set
    if( !is.null(xInverse) && is.matrix(xInverse)) {
      message("getting cached data")
    return(xInverse)
  }
  
## Get the matrix from our object
  matrixToSolve <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  xInverse <- solve(matrixToSolve)
  
  ## Set the inverse to the object
  x$setInverse(XInverse)
  
  ## Return the matrix
  xInverse
}
