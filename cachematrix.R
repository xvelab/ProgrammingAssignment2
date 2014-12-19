## Programming assignment 2
## Caching the inverse of a matrix

## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #Private variables of the function makeCacheMatrix
  m<-NULL
  
  #Private functions of the function makeCacheMatrix
  
  #Function set: replace the current matrix x with the new y
  set<-function(y) {
    #<<- operator assigns the value of y to x in the father function makeCacheMatrix variables
    #the same is for m
    x<<-y
    m<<-NULL
    
  }
  #Function get: returns the value of x, more than one line should have been between {}
  get<-function() x
  #Function setsolve: solve is a variable parameter of setsolve; not the function solve() of R.
  setsolve<-function(solve) m<<-solve
  #Function getsolve: just returns the value of m.
  getsolve<-function() m
  
  #This is what the function returns when it's called a variable of type makeCacheMatrix
  list(set=set,get=get, setsolve=setsolve, getsolve=getsolve)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #The function solve() is an R function, it calculates the inverse matrix
  m <- solve(data, ...)
  #The inverse matrix is saved with x$setsolve()
  x$setsolve(m)
  #The function returns the inverse matrix
  m
}
