## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix = creates  special 'matrix' object
  #   that can cache its own inverse
  # check x is square matrix
  dims = dim(x)
  if(dims[1] != dims[2]) {
    # not square 
    return('Not a square matrix!')
  }
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(s) inv <<- solve(s)
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #cacheSolve = computes the inverse of the special "matrix" 
  #  returned by makeCacheMatrix above. If the inverse has 
  #  already been calculated (and the matrix has not changed),
  #  then the cachesolve should retrieve the inverse from the cache.
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
