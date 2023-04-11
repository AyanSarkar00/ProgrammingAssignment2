## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#there are two functions makeCacheMatrix,cacheSolve
#library(MASS) is used to calculate inverse for non squared as well as square matrices
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                     #initializing inverse as NULL
  set <- function(y){             #define the set function to assign new
    x <<- y                       #value of matrix in parent environment
    inv <<- NULL                  #if a new matrix, reset inv to NULL
  }
  get <- function()x              #function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv {
                               inver<-ginv(x)
                               invers%*%x      #function to obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## This is used to get the cache data
##If the inverse has already been calculated and the matrix has not changed
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) ##gets cache data
  {
  inv <- x$getInverse()
  if(!is.null(inv)){             #checking whether inverse is NULL
                    message("getting cached data")
                    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)           #calculates the inverse value
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
