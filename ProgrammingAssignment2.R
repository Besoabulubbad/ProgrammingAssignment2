## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## MakeCacheMatrix function creat a special matrix object that can inverse it

makeCacheMatrix <- function(x = matrix()) {
matrix_inverse<-NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- solve
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)

}


## cacheSolve a function compute the inverse of a matrix if it's not exist whice returned by 
## makeChaceMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setreverse(m)
  m
 
}