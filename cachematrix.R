## SG Programming Assignment 2 lexical scoping


# Function that creates a list to:
# a) set the value of the matrix
# b) get the value of the matrix
# c) set the inverse of the matrix
# d) get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
      x <<-y
      inv<<-NULL
  }
  
  get<- function() x
  
  setinv <- function(inver) inv <<- inver
  getinv<- function() inv 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}  
  

## cahceSolve returns  the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if( !is.null(inv)) {
    message("getting cached data")
    return(inv)
    
  }
  
  data <-x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
