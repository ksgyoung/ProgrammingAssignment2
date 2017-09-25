## The functions below allow for the efficient management of matrix inverses 
## by allowing for the complex computation to be cached

## Returns a list of functions applicable to the given matrix parameter. 
## These functions include setters and getters for the matrix, 
## as well as setters and getters for the inverse

makeCacheMatrix <- function(primitive_matrix = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    primitive_matrix <<- y
  }
  get <- function() primitive_matrix
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the cache matrix object by furst checking if the object
## already has an inverse recorded. If there is an inverse it returns it. 
## If there isn't an inverse it calculates it, sets it for the object and returns it

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
