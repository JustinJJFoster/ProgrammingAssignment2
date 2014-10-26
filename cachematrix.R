## The following two functions, makeCacheMatrix and cacheSolve
## have been designed to compute the inverse of a matrix (whose
## inverse exists) and to cache said inverse within a variable
## in order to not have to recompute the inverse again once it
## has initially been calculated. 

## This function is used to cache the inverse of a matrix once 
## it has been computed. 

makeCacheMatrix <- function(x = matrix()) {
  
  ## Create a variable, m, within the makeCacheMatrix Environment
  
  m <- NULL
  
  set <- function(y){
  
  ## Superassign the value of y to the input variable, X  
    
    x <<- y
  
  ## If x changes above, m needs to be reinitialized to NULL  
  
    m <<- NULL
  }
  
  ## The get function is called upon in order to obtain the values
  ## of the matrix. cacheSolve will utilize this function. 
   
  get <- function() x
  
  ## Assinging the Inverse of the matrix to the variable m, in the 
  ## environment of makeCacheInverse
  
  setInverse <- function(Inverse) m <<- Inverse
  
  ## Attempting to find the value of the Inverse within the
  ## environment getInverse. If no such value exists, will 
  ## look outside the environment and to the parent environment 
  ## makeCacheMatrix for said value.
  
  getInverse <- function() m
  
  ## Enables the use of the listed functions in environments outside of 
  ## makeCacheMatrix. 
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function determines the inverse of a matrix if it has not
## already been computed. 

cacheSolve <- function(x, ...) {
  
  ## Calling the getInverse function and assigning its value to m.
  
  m <- x$getInverse()
  
  ## Checking if the inverse has already been computed and assigned to m
  
  if(!is.null(m))
  
  ## If TRUE, return the value of m, the Inverse. If FALSE, proceed to 
  ## solve for the inverse.
    
    {
    message("Obtaining the Cached Matrix")
    return(m)
    }
  
  ## Calling the get function in makeCacheSolve in order to populate
  ## the variable data with data in order to compute the inverse of the
  ## matrix. 
  
  data <- x$get()
  
  ## Computing the inverse with use of the solve function and assigning
  ## the inverse to m.
  
  m <- solve(data, ...)
  
  ## Caching the new inverse by updating m in the makeCacheSolve function
  
  x$setInverse(m)
  
  ## Printing the inverse
  
  m
}
