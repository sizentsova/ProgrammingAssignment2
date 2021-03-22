## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix takes a matrix as an argument
## and create a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set_mat <- function(y) {
    x <<- y
    n <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(solve) n <<- solve
  get_inv <- function() n
  list(set_mat = set_mat,
       get_mat = get_mat, 
       set_inv = set_inv, 
       get_inv = get_inv)
}


## The function cacheSolve computes the inverse of the 
## special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  n <- x$get_inv()
  if(!is.null(n)) {
    message("getting cached data") ## if the inverse matrix has already been calculated the function print "getting cached data"
    return(n)
  }
  mat <- x$get_mat()
  n <- solve(mat, ...) ## The function solve returns a matrix that is the inverse of 'x'
  x$set_inv(n)
  n
}

## testing the functions

test_matrix <- makeCacheMatrix(matrix(rnorm(16), 4, 4))
test_matrix$get_mat() 

inv_test_matrix <- cacheSolve(test_matrix)
inv_test_matrix

cacheSolve(test_matrix) ## run again to see the message "getting cached data"
