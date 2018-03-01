## This function creates a matrix that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## This function creates the inverse of the matrix created by
## the makeCacheMatrix function above. It can retrieve the 
## inverse from the cache if the invese has already been
## calculated.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        
}

A <- makeCacheMatrix(matrix((1:4),2,2))
A$get()
A$getinverse()
A$set(matrix((1:4),2,2))
cacheSolve(A)
A$getinverse()