## makeCacheMatrix - Stores and accesses an invertible matrix
## cacheSolve - Inverts the matrix. Result is obtained from cache if possible, else written to cache after calculation.


## Accessor function for matrix. The inverse is stored in m if cached.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseMatrix) m <<- inverseMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve does the work of accessing the inverted matrix (if already cached), or calculating it and caching it if not.
cacheSolve <- function(x, ...) {
  
  ## first try from cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  ## if not populated, calculate
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

  ## Return a matrix that is the inverse of 'x'
  m
}
