## Caching the Inverse of a Matrix

## use scope rules to save data 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## save data with scope rules
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  ## get data
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## If existed inverse, Return. Else cal and solve
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
