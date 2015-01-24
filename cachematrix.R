
## makeCacheMatrix contains function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
  x <<- y 
  inv <<- NULL 
  }
  get <- function() x 
  setinv <- function(solve) inv<<-solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the matrix. 
## It checks if the inverse has been already computed. 
## If yes, it prints the message and fetches  the cached data, 
## if not, it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
  data <- x$get() 
  inv <- solve(data) 
  x$setinv(inv) 
  inv 
}

