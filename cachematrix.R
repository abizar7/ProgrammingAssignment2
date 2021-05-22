## Put comments here that give an overall description of what your
## functions do

## Comprises of two functions makeCacheMatrix, makeCacheMatrix
##makeCacheMatrix consists of set, get, setinv, getinv



library(MASS)   ##I found that library(MASS) could be used to inverse squared as well as non squared matrices
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL  #inverse as NULL
  set <- function(y){
    x <<- y 
    inv <<- NULL
    
  }
  get <- function() {x}    #to get matrix x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){    #this is the function to get inverse of the matrix
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  
}


cacheSolve <- function(x, ...){ 
  inv <- x$getInverse()  ##this gets the inverse of x and assigns it to inv 
  ##so first we need to check if the inverse has already been cal, if already done then it can 
  ## get it from the cache and skip the required computation
  if(!is.null(inv)){
    message("obtaining cache data")
    return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}