## Inverse calculation of matrix is an expensive process.
## So it is benefitial to cache the inverse of a matrix to avoid calculation
## The below pair of fuctions stores a matrix, creates its inverse, 
## cache the inverse and retrieves the inverse from cache when required
## if inverse is not available in cache, it calculates the inverse

## the makeCacheMatrix function takes an invertible matrix as an agrument
## it creates a list of 4 functions
## set - to store the matrix
## get - to retrieve the matrix
## setInverse - to store inverse of the matrix
## getInverse - to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix 
## created by makeCacheMatrix function. If the inverse of the matrix is 
## cached, it retrieves the cached inverse
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("Inverse matrix cached. getting cached data")
          return(inv)
        }
        else message("Cache Empty. Calculating Inverse")
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
