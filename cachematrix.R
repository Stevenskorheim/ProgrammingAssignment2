## Finds and caches the inverse of the matrix

## Creates a cashe of the inverse of the matrix attached to the matrix variable

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse<-function() i
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}


## checkes for a cashed inverse, if none exists finds inverse

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cashed data")
    return(i) 
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
