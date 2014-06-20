## The prog for Assignment 2: lexical scoping


makeCacheMatrix <- function(x = matrix()) {
  ## Set the value of the matrix
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## Get the value of the matrix
  get<-function() x
  ## Set the inverse of the matrix
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  ## Get the inverse of the matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  ## Get the matrix
  m<-x$getmatrix()
  ## Check if the matrix is already in the cache
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## If the matrix is not in the cache, compute the inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
