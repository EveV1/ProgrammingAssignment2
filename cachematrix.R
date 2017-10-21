
# Function takes args matrix.
# Function creates getters and setters to matrix and its inverse matrix.
# The inverse matrix is NULL 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function takes args special variable created by makeCacheMatrix.
# If list contains inverse matrix function returns it.
# Else trys to calculate the inverse matrix, add it to variable and return inverse matrix.
# If matrix in args variable isn't square matrix, returns NULL and message about it.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (nrow(data)==ncol(data)){
    m <- solve(data, ...)
    x$setinverse(m)
  }else{
    message("matrix must be square matrix")
  }
  m      ## Return a matrix that is the inverse of 'x'
}

#test
x<-matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
bb<-makeCacheMatrix(x)
cacheSolve(bb)
cacheSolve(bb)
x<-matrix( c(5, 1, 0, 3,-1, 2), nrow=3, byrow=TRUE)
bb<-makeCacheMatrix(x)
cacheSolve(bb)
x<-matrix( c(5))
bb<-makeCacheMatrix(x)
cacheSolve(bb)