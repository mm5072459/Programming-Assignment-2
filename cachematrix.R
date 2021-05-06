## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function, makeCacheMatrix creates a matrix, which is a list with the function to:
#set the elements of the matrix
#get the elements of the matrix
#set the elements of the matrix inverse
#get the elements of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#The function below calculates the inverse of the matrix created with the function above.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_invert <- x$get()
  inv <- solve(matrix_invert, ...)
  x$setinverse(inv)
  inv
}
