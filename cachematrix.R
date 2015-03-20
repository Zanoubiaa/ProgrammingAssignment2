## The goal of the function is to create a cashing matrix and returns its inverse
## The main objective is to reduce time consuming when running inverse function

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(solve) inv <<- solve 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# The following function returns the inverse of the matrix.
# It first checks if the inverse has already been computed.
# If so, it gets the result and skips the computation.
# If not, it computes the inverse, sets the value in the cache via setinverse function.
#  This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  if (!is.null(inv)){

    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv = solve(data, ...)
  
  x$setinv(inv)
  
 inv
}

## Sample run:
#====================
## > M1 <- matrix(c(1, 1, 4, 0, 3, 1, 4, 4, 0), 3, 3)
## > M1c <- makeCacheMatrix(M1)
## > M1c$get()
##     [,1] [,2] [,3]
##[1,]    1    0    4
##[2,]    1    3    4
##[3,]    4    1    0

## No cache in the first run
## > cacheSolve(M1c)
##            [,1]        [,2]    [,3]
##[1,]  0.08333333 -0.08333333  0.2500
##[2,] -0.33333333  0.33333333  0.0000
##[3,]  0.22916667  0.02083333 -0.0625
## retrieves the inverse from the cache directly
## > cacheSolve(M1c)
## getting cached data
##            [,1]        [,2]    [,3]
##[1,]  0.08333333 -0.08333333  0.2500
##[2,] -0.33333333  0.33333333  0.0000
##[3,]  0.22916667  0.02083333 -0.0625
