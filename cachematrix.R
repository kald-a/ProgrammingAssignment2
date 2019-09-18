## converts matrix so that calculations are cacheable
## returns a list of functions to set the matrix, get the matrix, set the inverse
##allows you to check whether an inverse has already been computed

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##checks if inverse for that input has already been calculated (by seeing if it is still NULL)
##if already calculated, prints message and returns the cached inverse
##otherwise it retrieves the matrix, solves the inverse, sets the inverse value for that particular input, and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
