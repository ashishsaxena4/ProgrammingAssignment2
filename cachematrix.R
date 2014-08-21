## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
# input x as matrix
     i <- NULL 
#  i will be our 'inverse' and it's reset to NULL 		
# every time makeCacheMatrix is called
     set <- function(y) { #takes an input matrix
          x <<- y		# saves the input matrix
          i <<- NULL		#reset the inverse to null
     }
# this function returns the value of the original matrix
     get <- function() x 

# this is called by cachesolve() during the first cachesolve()
#  access and it will store the value using superassignment

     setinverse <- function(solve) i <<- solve

# this will return the cached value to cachematrix() on
#  subsequent accesses
     getinverse <- function() i

# This list is returned with the newly created object.
# It lists all the functions ("methods") that are part of the 
# object. If a function is not on the list then it cannot
#   be accessed externally.

     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The cacheSolve function is checking to see whether the inverse of x has  
## already been calculated and set.
## If it has been calculated then the cached answer is returned, avoiding
## unnecessary calculations. Otherwise the inverse is computed.

# the input is an object created by makecachematrix
cacheSolve <- function(x, ...) {

# accesses the object 'x' and gets the value of it's inverse
     i <- x$getinverse()

# if inverse was already cached (not NULL) ...
# ... send this message to the console
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
# we reach this code only if x$getinverse() returned NULL
     data <- x$get()
# if i was NULL then we have to calculate the inverse
     i <- solve(data, ...)
# store the calculated inverse in x (see setinverse() in #makecachematrix)
     x$setinverse(i)
# return the inverse to the code that called this function
     i
}

