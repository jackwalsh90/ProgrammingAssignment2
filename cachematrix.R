## These functions allow for the creating and caching matrix-inverse values. 

## This function created a matrix-like object that caches its own inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function computes the inverse of the object set above, or returns a cached inverse object if already calculated. 

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  mx <- x$get()
  inv <- solve(mx, ...)
  x$setinverse(inv)
  inv
}
