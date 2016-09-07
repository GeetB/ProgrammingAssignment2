## Put comments here that give an overall description of what your
## functions do

## The below funcion will create the matrix, then it will set and get the matrix. It will also set and get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  #Add matrix to cache
  inverse_new <- NULL
  set <- function(y) {
    x <<- y
    inverse_new <<- NULL
  }
  #Get matrix from cache
  get <- function() {
    x
  }
  #set the inverse of the matrix
  set_the_inverse <- function(inverse) {
    inverse_new <<- inverse
  }
  #Get the inverse of matrix
  get_the_inverse <- function() {
    inverse_new
  }
  
  list(set=set, get=get, set_the_inverse=set_the_inverse, get_the_inverse=get_the_inverse)
}

## The below function will calculate the inverse of matrix and it will also check if that inverse is cached or not. If it is cached then it will return it from cache.
cacheSolve <- function(x, ...) {
  #Ge the inverse of matrix
  inverse_new <- x$get_the_inverse()
  #if inverse_new is not NULL that means inverse is already cached
  if(!is.null(inverse_new)) {
    message("Get the cached inverse matrix....")
    return(inverse_new)
  }
  #if the above condition fails, then this is the first time to calculate inverse of matrix,the solve function will compute the inverse of sqaure matrix
  data <- x$get()
  inverse_new <- solve(data)
  x$set_the_inverse(inverse_new)
  inverse_new
}
