## Functions that create a matrix and return said matrix's inverse

## This function creates an object that stores a matrix and the matrix's inverse 

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  setmat <- function(b) {
    x <<- b
    a <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse) a <<- inverse
  getinv <- function() a
  list(setmat = setmat,
       getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse (or retrives it if already calculated and stored) of the matrix created by makeCacheMatrix  

cacheSolve <- function(x, ...) {
  a <- x$getinv()
  if (!is.null(a)) {
    message("retriving stored calculation")
    return(a)
  }
  smat <-x$getmat()
  a <- solve(smat, ...)
  x$setinv(a)
  a
}
