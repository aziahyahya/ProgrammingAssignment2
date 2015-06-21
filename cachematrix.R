## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a constructor function that stores
## 4 functions that can contain the original and inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  ## First, we set the matrix inverse to be used in this environment
  myinverse <- NULL
  
  ## We create a function to set a new matrix if needed
  ## Everytime a new matrix is set we set the inverse outside this
  ## function to NULL
  set <- function (y){
    x <<- y
    myinverse <<- NULL
  }
  
  ## assigns a function that stores the matrix
  get <- function ()x
  
  ## assigns a function that replaces inverse with input
  ## the <<- operator will ensure the inverse outside the function
  ## environment is assigned and this follows the lexical scoping rules.
  ## Refer to the link below
  ## https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html
  setinverse <- function(matinverse) myinverse <<- matinverse
  
  ## assigns a function o retrieve the current value of myinverse
  getinverse <- function () myinverse
  
  ## returns a list that contains the functions
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function will receive an input that contains funtions from the
## makeCacheMatrix, check to see if an inverse cache exists returns the
## cache if it exists, and calculates and returns an inverse otherwise


cacheSolve <- function(x, ...) {
  ## The dollar sign is used to extract named elements
  ## Hence, x$getmean will find "getinverse", in the input "x"
  myinverse <- x$getinverse()
  
  ## if the mean is not null cacheinverse retrieves the calculated
  ## inverse matrix. Return will skip the rest of the code
  if(!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  
  ## if the mean in null, retrieve the get function from the input vector
  ## the retrieved function will pass the stored numeric data to "data"
  matrixdata <- x$get()
  
  ## we solve the inverse
  myinverse <- solve(matrixdata)
  
  ## we retrieve the setmean function and assign a value to m
  ## the <<- operator will ensure m outside the function environment is assigned
  ## and this follows the lexical scoping rules. Refer to the link below
  ## https://stat.ethz.ch/R-manual/R-devel/library/base/html/assignOps.html
  x$setinverse(myinverse)
  
  myinverse    
}
