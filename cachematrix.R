
## The following functions cache the inverse of a matrix. If the inverse of a matrix 
## is already computed then it gets it from the cache. If it's not, then it gets computed
## using the Solve() function.
## For this assignment, we assume that the matrix supplied is always invertible.

## makeCacheMatrix is a list of 4 functions (set, get, setinverse, getinverse)
## which are used to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## In order to compute the inverse of the matrix, solve() function is used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
