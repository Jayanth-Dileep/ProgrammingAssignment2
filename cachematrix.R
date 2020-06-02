##These pair of fucntions are used to assign a special matrix and computes the inverse of the special "matrix" returned by makeCacheMatrix 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
  {
  ## Initialize the inverse property
  i <- NULL
  
  ## Asigning to the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
}

get <- function() {
  ## Return the matrix
  m
}

##Sets the Inverse of the matrix
setInverse <- function(inverse) {
  i <<- inverse
}

## Gets the inverse of the matrix
getInverse <- function() {
  ## Return the inverse property
  i
}

## Return a list of the methods
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ##asigning inverse of 'x' to 'm'
  m <- x$getInverse()
  
  ## checks the inverse matrix and prints the matrix if it is present 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculating the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Asigning to a object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
