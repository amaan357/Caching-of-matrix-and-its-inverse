## This function takes a matrix as its input.
## This fuction has a list of fuctions inside that set the value of the matrix, get the matrix,set its inverse and get the inverse
## This function is used to cache the inverse of the matrix and the original input matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function take sthe output of the previous function makecachematrix as its input.
## It checkes if the inverse is already cached and returns it.
##if not it caculates the inverses, caches it and returned the calculated inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
