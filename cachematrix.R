## Chris Cowan

## Create a matrix object with a cache to store the inverse
## This object is strictly a bucket.  Calculation must be performed by 
## an outside method.

# Separate setter and getter for both the matrix and the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Solve Cache Matrix object.  Only calculates new result if the inverse is not
## already cached.   If calculation is performed, cache result for future calls

# This function assumes that the matrix is always invertible.  solve() call will 
# throw an error otherwise.  For example, when the matrix is singular.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

# Example:
# > x <- makeCacheMatrix(rbind(c(1:3),c(4:6),c(0,1,3)))
# > x$get()
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6
# [3,]    0    1    3
# > cacheSolve(x)
# [,1]       [,2] [,3]
# [1,] -3.000000  1.0000000    1
# [2,]  4.000000 -1.0000000   -2
# [3,] -1.333333  0.3333333    1
# > cacheSolve(x)
# getting cached data
# [,1]       [,2] [,3]
# [1,] -3.000000  1.0000000    1
# [2,]  4.000000 -1.0000000   -2
# [3,] -1.333333  0.3333333    1
# > x$setinverse(NULL)
# > cacheSolve(x)
# [,1]       [,2] [,3]
# [1,] -3.000000  1.0000000    1
# [2,]  4.000000 -1.0000000   -2
# [3,] -1.333333  0.3333333    1
# > x$get() %*% cacheSolve(x)
# getting cached data
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
