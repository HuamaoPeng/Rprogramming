## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly.Below is a pair of functions 
##that cache the inverse of a matrix.


##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of 
##the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
   ## Return a matrix that is the inverse of 'x'
    i
}

