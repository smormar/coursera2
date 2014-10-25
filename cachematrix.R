## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<-y
    a<<- NULL
  }
  ## get the value of the matrix
  get <- function () x
  ## set the inverse of the matrix
  setinvmat <- function(solve) a <<- solve
  getinvmat <- function () a
  ## get the inverse of the matrix
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## get the inverse of the matrix
  a <- x$getinvmat()
  ## check if there is the matrix
  if(!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  ## if not: get the inverse of the matrix
  data <- x$get()
  a <- solve(data, ...)
  ## set the inverse of the matrix
  x$setinverse(a)
  a
}
