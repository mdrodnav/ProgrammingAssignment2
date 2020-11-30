## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatrix consist of set, get, setInverse and getInverse
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}  #this is to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv} #with this we obtain the inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

## this is to get cache data
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)  #returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)  #calculates inverse value
  x$setInverse(inv)
  inv  #returns a matrix inverse to x
}

