# Matrix inversion is usually a costly computation and there may be some benefit to Caching the inverse of a matrix rather than compute it repeatedly. 
#Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.


#makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)

}


# The following function calculate the inverse of the matrix. 
# If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
if(!is.null(i)) {
  message("getting cached data.")
  return(i)
}
data <- x$get()
i <- solve(data)
x$setinverse(i)
i

}
