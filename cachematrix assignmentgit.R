## The makeCacheMatrix function takes a matrix as an argument
## and returns a list of functions that can be used to cache update
## the matrix and its inverse.
## the cacheSolve function is used to save the inverse of the matrix so 
## it can be called as needed, saving computational power of needing to calcualte 
## the inverser over and oveer.

## Takes a matrix as an argument, and thereafter allows the functions 
## get() - to gt the matrix, set() - to update the metrix to a new one
## getinv()- if an inverser has already been cached, this is used to return it
## setinv()- if a cached inverse does not exist, this will calcualte the inverse
## and cache it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function(){x}
  setinv <- function(inversem) inv <<- inversem
  getinv <- function(){inv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## checks if a cached inverse already exists, if not calculates and caches it.
## when the function is run for the first time, it will calculate the inverse
## on the second call and thereafter, it will simply return the saved inverse of matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    print("getting inverted matrix")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinv(inv)
  inv
}
