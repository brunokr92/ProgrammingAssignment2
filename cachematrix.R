## Put comments here that give an overall description of what your
## functions do

#The special matrix is created here with methods for get, get inverse
#and set inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x<<- y
    mat<<- NULL
  }
  get<-function() x
  setinv <-function(inv) mat<<- inv
  getinv <-function() mat
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#The cache solve function attempts to retrieve the cached matrix and 
#if not found, it solves it through the solve function.
cacheSolve <- function(x, ...) {
  m<- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m       
}
