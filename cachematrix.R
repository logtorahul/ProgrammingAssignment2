
##This function creates a special "matrix" object that can cache its inverse.
## Rahul 9/27/2015
makeCacheMatrix <- function(x = matrix()) 

{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
## Rahul 9/27
cacheSolve <- function(x, ...) 
  
{
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  
  if (!is.null(invMatrix))
  ## If available in Cache
  {
    message("getting cached data..")
    return(invMatrix)
  }
  ##if not availble in cache
  
  message("Computing inverse..")
  mat <- x$get()
  
  tryCatch( { 
    invMatrix <- solve(mat, ...)
  },
  
    error = function(e) {
      message("Error..")
      message(e)
      
      return(NA)
    },
    warning = function(e) {
      message("Warning..")
      message(e)
      
      return(NA)
    },
    finally = {
      
    x$setInverse(invMatrix)
    })
  
  return(invMatrix)
}



