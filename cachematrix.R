## Put comments here that give an overall description of what your
## functions do

## Since computation of the inverse of a matrix can be costly, these functions are
## used to compute the inverse as well as cache it for future use

## Write a short comment describing this function

## The function is used for creating a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y){
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() {x}
  set_inverse <- function(inverse) {matrix_inv <<- inverse}
  get_inverse <- function() {matrix_inv}
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## Write a short comment describing this function

## The function computes the inverse of the matrix created by makeCacheMatrix
## In case the computation has already been performed, it simply retrieves 
## the inverse matrix from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inv <- x$get_inverse()
  if(!is.null(matrix_inv)){
    message("retrieving cached data")
    return(matrix_inv)
  }
  mat <- x$get()
  matrix_inv <- solve(mat,...)
  x$set_inverse(matrix_inv)
  matrix_inv
}
