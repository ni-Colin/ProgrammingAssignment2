## The functions makeCacheMatrix and cacheSolve allow for the creation of a "matrix"
## where the inverse may be cached and recalled for future computations rather than
## being recalculated when needed again.

## makeCacheMatrix creates a "matrix" that is actually a list containing functions
## that can change the values of the matrix and set and recall the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL        
  }         
  get <- function() x
  setinverse <- function(inverse) inv=inverse
  getinverse <- function() inv
  
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## cacheSolve calculates the inverse of the makeCacheMatrix subject if it has not
## already been computed, or returns the cached value if it has been previously
## calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse matrix!")
    return(inv)
  }
  tempData = X$get()
  inv <- solve(tempData)
  X$setinverse(inv)
  inv
}
