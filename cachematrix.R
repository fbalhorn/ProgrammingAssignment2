##Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.
##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the matrix' inverse
##    4. get the value of the matrix' inverse


## creates a cacheMatrix and returns a list of functions to handle the cacheMatrix
makeCacheMatrix <- function(x = matrix()) { 
  matrixInverse <- NULL               			## setting the result to NULL, i.e., initializing the funtion
  set <- function(y) {               			## assign a new value to the cacheMatrix and initialize the inverse value with NULL
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x                                       ## funtion to get the value of the matrix
  setinverse <- function(inverse) matrixInverse <<- inverse ## function to store the inverse 
  getinverse <- function() matrixInverse                    ## function to get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)					## return the list of needed functions
}                                    			

## Return a matrix that is the inverse of 'x'        
cacheSolve <- function(x, ...) {	
  matrixInverse <- x$getinverse()    		 	## calculate the inverse of an input CacheMatrix
  if(!is.null(matrixInverse)) {       			## in case a value for the inverse is cached... 
    return(matrixInverse)						## ...it is returned
  }
  data <- x$get()                     			## in case no value for the inverse is cached, it is retrieved from the cacheMatrix and temporarily stored in "data"
  matrixInverse <- solve(data,...)    			## the inverse is calculated on "data"...
  x$setinverse(matrixInverse)         			## ... and stored in the cacheMatrix via the setInverse function
  matrixInverse                       			## the calculated value is returned
}

