#
# The functions contained in this file implement a mechanism for
# calculating the inverse of a matrix and storing the result in
# cache for later retrieval.
#


#
# The makeCacheMatrix function implements a mechanism for caching the results
# of matrix inversion.
#
makeCacheMatrix <- function (original = numeric()) {
  # The inverse of the original matrix.
  inverse <- NULL
  
  # Set the initial values for the matrix.
  set <- function(initial) {
    original <<- initial
    inverse <<- NULL
  }
  
  # Return the original matrix.
  get <- function() original
  
  # Set/Store the inverse of the original matrix.
  setinverse <- function(matrix) inverse <<- matrix
  
  # Return the stored inverse.
  getinverse <- function() inverse
  
  # Set the values of the vector, and return it.
  list(set = set,
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


#
# The cacheSolve function returns the inverse of a matrix.
#
# If the inverse was previously calculated, retrieves and returns the inverse
# from cache. Otherwise, calculates and returns the inverse, caching the
# result in case it is needed later.
#
cacheSolve <- function(myMatrix, ...) {

  
  # Attempt to retrieve the inverse from cache.
  inverseMatrix <- myMatrix$getinverse()
  
  if(!is.null(inverseMatrix)) {
    
    # Inverse already exists, so return it.
    message("returning cached data")
    return(inverseMatrix)
  }
  
  # We get here if the inverse has not yet been calculated.
  
  # So retrieve the matrix.
  originalMatrix <- myMatrix$get()
  
  # Calculate the inverse.
  inverseMatrix <- solve(originalMatrix)
  
  # Cache the result for later use.
  myMatrix$setinverse(inverseMatrix)
  
  # Return the inverse.
  inverseMatrix
}

