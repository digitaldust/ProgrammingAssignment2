## This function calculates the inverse of a matrix,
## taking into account previously computations. If that matrix has been
## already inverted, the cached result is returned, otherwise the function
## calculates the inverse of the given matrix.
## USAGE:
## # first provide a squared matrix, which is invertible
## m <- matrix(seq(1,4),nrow=2,ncol=2)
## # then call makeCacheMatrix on the matrix
## cacheMatrix <- makeCacheMatrix(m)
## # finally, cacheSolve will compute the inverse of the matrix, if
## # nothing has been computed yet, otherwise it will return the previously
## # computed result - in this latter case, a text message will signal that
## # the returned result was cached
## cacheSolve(cacheMatrix)
## # you can change the matrix using
## cacheMatrix$set(anewmatrix)

## this function takes a matrix as input
makeCacheMatrix <- function(x = matrix()) {
  ## set to NULL the inverse of x
  m <- NULL
  ## a setter method that initialize in this environment the value of
  ## the matrix x to y and its inverse to NULL
  ## this is useful to change the matrix without creating again the object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## a getter method that return the matrix received as input
  get <- function() x
  ## a setter method that store in m in this environment the inverse
  ## of x which is computed by cacheSolve()
  setinverse <- function(inverse) m <<- inverse
  ## a getter method to return the inverse of x
  getinverse <- function() m
  ## methods we can call on this object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function takes as input an object x
cacheSolve <- function(x, ...) {
  ## then call the method 'getinverse' to retrieve
  ## a previously cached value
  m <- x$getinverse()
  ## if the method returns not NULL
  if(!is.null(m)) {
    ## then it means we already computed the inverse
    message("getting cached data")
    ## and we simply return it - using return() we break execution here
    return(m)
  }
  ## we call the method 'get' which returns the matrix contained in x
  data <- x$get()
  ## we invert the matrix using solve()
  m <- solve(data, ...)
  ## we store back the value for further computations
  x$setinverse(m)
  ## and we return it also as a result to be printed on screen
  m
}
