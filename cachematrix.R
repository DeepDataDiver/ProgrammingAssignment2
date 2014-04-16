# This second programming assignment will require you to write an R function 
# is able to cache potentially time-consuming computations

# In this Programming Assignment will take advantage of the scoping rules of 
# the R language and how they can be manipulated to preserve state inside of 
# an R object.

# The first function, makeVector creates a special "vector", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean 
# 4. get the value of the mean
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# The following function calculates the mean of the special "vector" created with 
# the above function. However, it first checks to see if the mean has already been 
# calculated. If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in 
# the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# The following function calculates the mean of the special "vector" created 
# with the above function. However, it first checks to see if the mean has 
# already been calculated. If so, it gets the mean from the cache and skips 
# the computation. Otherwise, it calculates the mean of the data and sets 
# the value of the mean in the cache via the setmean function

## Assignment: Caching the Inverse of a Matrix

# Write the following functions:

# makeCacheMatrix: This function creates a special "matrix" object that can 
#                  cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" 
#             returned by makeCacheMatrix above. If the inverse has already 
#             been calculated (and the matrix has not changed), then 
#             the cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the solve function 
# in R. For example, if X is a square invertible matrix, then solve(X) 
# returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

# In order to complete this assignment, you must do the following:

# 1. Clone the GitHub repository containing the stub R files at 
#    https://github.com/rdpeng/ProgrammingAssignment2
# 2. Edit the R file contained in the git repository and place your solution
#    in that file (please do not rename the file).
# 3. Commit your completed R file into YOUR git repository and push your git
#    branch to your GitHub account.
# 4. Submit to Coursera the URL to your GitHub repository that contains the 
#   completed R code for the assignment.

##/*------------------------------------------------------------------*/
##/* Assignment 2 starts here                                         */
##/*------------------------------------------------------------------*/

## Assignment: Caching the Inverse of a Matrix
## This Programming Assignment will take advantage of the scoping rules of 
## the R language and how they can be manipulated to preserve state inside of 
## an R object.

## This function: 
## 1) Receives a source matrix argument and 
## 2) caches the inverse of the source matrix (created using the solve method) 
## returns a list object that sets/gets the source and inverse matrixes
## Its assumes the source matrix is always invertible

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # initializes an empty default inverse matrix
  inv <- matrix(nrow=0, ncol=0)
  
  # sets the source matrix 
  set <- function(y) {
    # assigns to x the value of y from external environment 
    x <<- y
    
    # initializes inverse matrix in external environment 
    inv <<- matrix(nrow=0, ncol=0)
  }
  
  # gets the source matrix
  get <- function() x
  
  # sets inverse matrix using solve method 
  setinvmatrix <- function(solve) inv <<- solve
  
  # return inverse matrix 
  getinvmatrix <- function() inv
  
  # creates a named list (so one can be access items by name)
  list(set = set, 
       get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## This function 
## 1) Receives the special "matrix" returned from makeCacheMatrix above   
## 2) Returns the inverse of the special "matrix"returned by makeCacheMatrix above
##      a) Computes the inverse of the special "matrix"returned by makeCacheMatrix 
##         above and stores it in the cache.  
##      b) If the inverse has already been calculated (and the matrix has not changed), 
##         then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {  
  # gets inverse matrix 
  inv <- x$getinv()
  
  # checks if inverse matrix already exists 
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    
    # returns cached inverse matrix 
    return(inv)
  }
  
  # there is no cached inverse matrix 
  # so, it computes it 
  
  # gets source matrix as input 
  data <- x$get()
  
  # computes inverse matrix using source matrix as input 
  inv <- solve(data, ...)
  
  # sets the cache with computed inverse matrix 
  x$setinvmatrix(inv)
  
  # returns inverse matrix  
  inv
}
