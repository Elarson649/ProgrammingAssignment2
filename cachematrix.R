## These functions take a matrix and find its inverse; if the inverse has already been calculated, we return the stored value.

## This function takes a matrix as input and returns a list of various functions to be used to in the inverses' calculation and storage.

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)){print("Object entered is not matrix")}
  i <- NULL
  get <- function() {x}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  list (get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function takes the list created above and returns the inverse, if possible, of the matrix initially entered. If we've calculated the inverse already, we'll return the stored value. If no stored value is available, we will calculate the inverse and store it for future use.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
