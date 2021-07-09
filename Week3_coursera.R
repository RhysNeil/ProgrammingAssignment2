## Set descriptions that will give a general representation of what a specific
## roles do
## These tasks recorded in a limited fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment; week beginning July 9, 2021; GitHub user: 

## put down a brief description relating to this function

makeCacheMatrix <- function(x = matrix()){
  ## This function creates a special "matrix" object that can cache its inverse
  
  makeCacheMatrix <- function(midoriya= matrix()) { ## define the argument with default mode of "matrix"
    itodori <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
      midoriya <<- y                             ## value of matrix in parent environment
      itodori <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() midoriya                     ## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) itodori <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() itodori                     ## obtain the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
    ## to the functions with the $ operator
  }
  
  
  ## Put down a short description relating to this role
  ## This specific role calculates the opposite of the special "matrix" returned by makeCacheMatrix above.
  ## If the opposite has been computed (and the matrix does not change),
  ## then cacheSolve will most likely recover the opposite from the cache
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    itodori <- x$getinverse()
    if(!is.null(inv)) {
      message("already got the data")
      return(itodori)
    }
    data <- x$get()
    itodori <- solve(data, ...)
    x$setinverse(inv)
    itodori
  }
  
  