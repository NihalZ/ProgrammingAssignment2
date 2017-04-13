##  Caching the Inverse of a Matrix

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the value of the inverse
##get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set,get=get, setinverse=setinverse, getinverse= getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inverse<- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached inverse")
    
    return(inverse)
    
  }
  data <- x$get()
  inverse<- solve(data)
  x$setinverse(inverse)
  inverse
}
