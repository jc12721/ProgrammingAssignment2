## Functions to compute a Matrix Inverse with Caching 
# Caching Function for a Matrix, this function returns a
# list of getters and setters for the Matrix as well as
# placeholders for the Matrix Inverse calculation.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(inverse) m <<- inverse
  getmatrixinverse <- function() m
  list(set = set, get = get, 
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}	
## Return a matrix that is the inverse of 'x'	

cacheSolve <- function(x, ...) {
  m <- x$getmatrixinverse()
  if (!is.null(m)) {
    message("getting cached matrix inverse")
    return (m)
  }
  matrix <- x$get()
  m <- solve(matrix) 
  x$setmatrixinverse(m)
  m
}	


m <- matrix(rpois(25, lambda=4),5)  
cache <- makeCacheMatrix(m)         
cache$get()                         
cacheSolve(cache)                   
cacheSolve(cache)
t <- system.time(cacheSolve(cache))
t

