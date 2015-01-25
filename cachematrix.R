## The two following functions have as purpose to cach the inverse of 
## a matrix. 

## makeCacheMatrix creates a matrix containing a function that:
## Sets the value of the matrix
## Gets the value of the matrix
## Sets the value of the inverse of the matrix
## Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## CacheSolve returns the inverse of the matrix with the following steps
## 1. Checks if the inverse has already been computed
## 2. If yes, gets the result - no further computation
## 3. If not, computes the inverse of the matrix
## 4. Sets the value in the cache
## 5. Returns the result

cacheSolve <- function(x, ...) {

  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
