## The makeCacheMatrix function takes in a matrix and then creates four functions
## that cache the original matrix and the inverse of the matrix
## the cacheSolve function determines if an inverse was already created for the input matrix
## and returns either the cached version or runs the solve() function to determine the inverse
makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL                       ## initialize the Inverse value
## sets the input matrix into cache     
     set_matrix <- function(matrix_x){ 
       x <<- matrix_x
       inv <<- NULL
     }
     
## get_matrix returns the input matrix     
     get_matrix <- function(){
       x
}
## set the Inverse of the matrix      
     setInv <- function (inverse){
       inv <<- inverse
     }
## get the inverse value
     getInv <- function() {
       inv
     }
## creates the list of functions that can be referenced by the cacheSolve function     
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix, 
       setInv = setInv, 
       getInv = getInv)
}
## cacheSolve takes in a matrix and using the functions defined in makeCacheMatrix
## determines if the inverse was previously generated and written into cache
## if it was, cacheSolve returns the cached version, otherwise it generates
## the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInv() ##use the makeCacheMatrix function to populate an internal variable
  
  ## determine if the inverse exists in cache, if it does, return it
  if (!is.null(inverse_matrix))
  {
    message("getting cached data")
    return(inverse_matrix)
  }
##inverse did not exist in cache so grab the original matrix from makeCacheMatrix
##assign to an internal variable  
  internal_matrix <- x$get_matrix()
  inverse_matrix <- solve(internal_matrix) ##generate the inverse
##assign the inverse to the holding value so next time it runs, it will be pulled from cache  
  x$setInv(inverse_matrix) 
##return the inverse
    return(inverse_matrix) 
  
}
