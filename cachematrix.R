## Cache an inverve of a matrix for future use
## Assume matrix is invertible

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      ## Set the metrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## return the matrix
      get <- function() x
      
      ## Set inverse of the matrix
      setinverse <- function(inverse) m <<- inverse
      
      ## Reutrn the cached inverse matrix
      getinverse <- function() m
      
      ## Return the matrix as an object with functions attached
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      ## No, the matrix hasn't changed.
      ## Check if we have a chached inverse
      m <- x$getinverse()
      
      ##  If inverse exists in cache, return it
      if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }      

      ## Otherwise, cache the inverse and return it
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}
