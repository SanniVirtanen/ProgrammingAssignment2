## Functions below caches an inverse of a matrix


# Example -----------------------------------------------------------------

## An example usage
# matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
# matrix2 <- makeCacheMatrix(matrix)

# cacheSolve(matrix2)
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

# cacheSolve(matrix2)
# getting cached inverse of a matrix
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5



# Functions: assignment 2 -------------------------------------------------

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Set inverse to be null
  inv <- NULL
  
  # set & get function for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # set & get inverse function for matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # set list of functions for the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # returns inverse of a matrix if it has already been computed
  if(!is.null(inv)) {
    message("getting cached inverse of a matrix")
    return(inv)
  }
  
  # computes inverse of a matrix
  matrix <- x$get()
  inv <- solve(matrix, ...)
  
  # cache inverse of a matrix
  x$setinverse(inv)
  
  ## Return inverse of a matrix x
  inv 
}
