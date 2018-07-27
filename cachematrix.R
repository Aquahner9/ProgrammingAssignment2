##A pair of functions for computing the inverse of a matrix, using cache data first and "falling back" on the
## (more expensive) matrix computation operations if a cached value is not found.
##Expected function usage is something like this:
## my_matrix <- makeCacheMatrix(diag(3))
## cacheSolve(my_matrix)

#makeCacheMatrix takes a matrix as an argument and returns a list of methods that will be used to find the matrix inverse.
##Note that here we only need the "getmatrix" and "setinv" functions; the other functions provided in the example 
##are useful for debugging purposes, but not strictly necessary.
makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  getmatrix <- function() x
  setinv <- function(inverse) inv <<- inverse
  list(getmatrix = getmatrix,
       setinv = setinv)
}

##cacheSolve takes the list returned by makeCacheMatrix and attempts to find the cache. 
##If the cache is not found, it computes the inverse. 
cacheSolve <- function(x, ...) {
  print("Trying to find inverse in cache")
  if(!is.null(inv)) {
    message("Cached value found, returning cache")
    return(inv)
  }
  message("Cached value not found. Computing matrix inverse.")
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

