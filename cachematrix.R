## These functions allow a matrix to be stored when 
## solved for inverse if not already stored.

## makeCacheMatrix stores a matrix for later access.

makeCacheMatrix <- function(x = matrix()) {
  storedMatrix <- NULL  
  
  set <- function(y) {
    x <<- y
    storedMatrix <<- NULL
  }
  
  get <- function(y) {
    return (x)
  }
  
  setMatrix <- function(newMatrix){
    storedMatrix <<- newMatrix
  }
  
  getMatrix <- function() {
    return(storedMatrix)
  }
  
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
  
}


## cacheSolve looks for a matrix stored with makeCacheMatrix and then inverts it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("Getting cached Matrix")
    return(m)
  }
  
  data <- x$get() 
  m <- solve(data, ...)
  x$setMatrix(m)
  m
  
}