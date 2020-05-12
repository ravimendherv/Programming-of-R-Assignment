## geting the Inverse of a Matrix.
## This function creates a special "matrix" object that can cache its inverse.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## here the NULL value is assign to the inv variable
  ## belows function result assign into the set variable
  set <- function(y) {
    x <<- y
  ## The <<- operator which can be used to assign a value to an object in an environment 
  ## that is different from the current environment.
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
## Here this Function will return the Vector into the get variable
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
## Here the " if " Condtion will Check the inv variable is Null or not
## inv is Null then message() function will sent the message tht "getting cached data"
## And inv is return to the cacheSolve() function.
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

