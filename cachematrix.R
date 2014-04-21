## makeCacheMatrix and cacheSolve allows to create an object that stores a matrix with its inverse, in order to reuse the latter
## value more times without having to recompute it.

## makeCacheMatrix creates an object that store a matrix, passed as input parameter, with its inverse (computed later). It allows
## to reuse the inverse by caching its value, saving computing time when the inverse it is know to be required more times.
## The returned object has 4 functions:
### set: set the matrix to be cached
### get: get the cached matrix
### setinverse: cache the computed inverse of the cached matrix
### getinverse: get the computed inverse, without having to recompute it
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes and stores the value of the inverse of the matrix cached in an object created by the makeCachedMatrix 
## function (passed as argument), storing it in the object itself. If the inverse has been already computed, it returns the value
## directly. Otherwise, the value is computed, stored and returned to the caller of the function.
cacheSolve <- function(x) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}


# ## unit tests
# mat=matrix(c(3,1,2,7,8,9,4,5,6),nrow=3,ncol=3)
# matInv = solve(mat)
# cacheMat = makeCacheMatrix(mat)
# cacheMat$get()
# if (!all(matInv == cacheSolve(cacheMat)))
#   message("test not passed")
# if (!all(matInv ==  cacheMat$getinverse()))
#   message("test not passed")
# if (!all(matInv ==  cacheSolve(cacheMat)))
#   message("test not passed")
