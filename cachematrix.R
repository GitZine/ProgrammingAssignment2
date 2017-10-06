## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix"
#object that can cache its inverse.


makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv_m) i <<- inv_m
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
#cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), then 
#the cachesolve should retrieve the inverse from the cache

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
