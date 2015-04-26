## makeCacheMatrix creates a 'matrix' object with additional functionality,
## and cacheSolve calculates the inverse of a 'matrix' object, storing it
## if it is not already

## makeCacheMatrix makes and returns a special 'matrix' object 
## which is a list with functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## declare m variable to store inverse
  m <- NULL
  
  ## 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## 2. get the value of the matrix
  get <- function() x
  
  ## 3. set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  
  ## 4. get the value of the inverse
  getinverse <- function() m
  
  ## returns list that is special 'matrix' object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix, unless it is stored
## already in a matrix object, in which case the inverse is retrieved
## from that object

cacheSolve <- function(x, ...) {
  
  ## checks matrix object for whether inverse has already been calculated
  m <- x$getinverse()
  
  ## if matrix inverse already has been calculated, displays message
  ## and returns that inverse and exits function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if matrix inverse has not already been calculated, retrieves matrix
  ## and solves for inverse, storing it in m and caching that inverse in
  ## the matrix object with setInverse()
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## Returns a matrix that is the inverse of 'x'
  m
}
