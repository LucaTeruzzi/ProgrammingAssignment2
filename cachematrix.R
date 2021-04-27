## These functions are useful to retrieve the inverse of a given square invertible matrix.
## Since this kind of computation is generally costly and time-expensive, the aim is to
## write a code able both to compute the inverse of the given matrix ex-novo and to retrieve 
## it from the cache if the original matrix has not changed.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## This function is used to create a matrix object to calcuate the inverse of a given 
## square and invertible matrix.
## The function is written with global assignment so that it can be used as a reference.

makeCacheMatrix <- function(x = matrix()) {
 
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() { x }
  set_inverse <- function(inv) { mat <<- inv }
  get_inverse <- function() { mat }
  
  l <<- list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## This function computes the inverse of the matrix defined by the previous method
## ('makeCacheMatrix'); if the inverse has already been calculated (and the matrix has not changed),
## then the 'cacheSolve' function should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  
  inv_matrix <- x$get_inverse()
  
  if(!is.null(inv_matrix))
  {
    message("Inverse matrix already existing: getting cached data...")
    return(inv_matrix)
  }
  
  data <- x$get()
  inv_matrix <- solve(data)
  x$set_inverse(inv_matrix)
  inv_matrix

  }


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
