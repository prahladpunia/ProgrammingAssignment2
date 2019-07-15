## ANSWER For makeCacheMatrix
##  Inverse Matrix Pair
## makeCaheMatrix and cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## INVERSE MATRIX CACHED 
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv))
  {
    print("retrieving Cached values")
    return(inv)
  }
  data<-x$get()
  inv<- solve(data,...)
  x$setinv(inv)
  inv
  
}

