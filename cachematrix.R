## These two functions compute the inverse of a square matrix. First one, makeCacheMatrix,
## stores and computes the matrices. The second one, cacheinverse, either calls already computed inverse matrix,
## or computes the inverse. Detailed description of functions is given at appropriate lines.



makeCacheMatrix <- function(x = matrix()) {
  # Stores matrix and its inverse in cache. The input have to be a squared matrix
  # (same number of rows and collumns).
  
  
  s <- NULL 
  
  setmatrix <- function(y) {                 #enables to set matrix without computing its inverse
                            x <<- y
                            s <<- NULL
                            }
  getmatrix <- function() x                  #enables to call for input matrix
  setinverse <- function(solve) s <<- solve  #compute inverse of input matrix
  getinverse <- function() s                 #enables to call for output, inverted, matrix
  
  list(getmatrix = getmatrix,                #lists created object so they can be invoked
       setmatrix = setmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}




cacheinverse <- function(x, ...) {
  # Returns a matrix that is the inverse of 'x' or takes the inverse from cache
  
  
  s <- x$getinverse() #invoke inversed matrix from cache
  
  #if there actually is inversed matrix in cache, print in on screen:
  if(!is.null(s)) { 
                  message("getting cached data")
                  return(s)
                  }
  
  
  #if there is no inversed matrix in cache, compute it:
  else
      data <- x$getmatrix()    #takes matrix from previous run of makeCacheMatrix function
      s <- solve(data, ...)    #solves the matrix and prints it
      x$setinverse(s)
      s
}