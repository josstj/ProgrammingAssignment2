## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly.  This code includes a pair of functions 
##that cache the inverse of a matrix.

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  #x is intitialized as an argument
  m <- NULL                                  #m is initialized as a Null matrix, to be filled later
  set <- function(y) {                       
    x <<- y                                  # assigns y to x in parent environment
    m <<- NULL                               # assigns m to NULL in parent environment
  }
  get <- function() x                        #retrieves x from parent environment
  setsolve <- function(solve) m <<- solve    # assigns 'solve' to m in parent environment
  getsolve <- function() m                    #defines the getter for the inverse/solve function of m
  list(set = set,                             #gives the name 'set' to the set() function defined above
       get = get,                             #gives the name 'get' to the get() function defined above
       setsolve = setsolve,                   #gives the name 'setsolve' to the setsolve() function defined above
       getsolve = getsolve)                   #gives the name 'getsolve' to the getsolve() function defined above
}

## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {               # x is initialized as an argument, and elipsis (...) allows the caller to pass additional arguments into the function.
  m <- x$getsolve()                          #function attempts to retrieve the inverse from the object passed in as the argument
  if(!is.null(m)) {                            #checks to see whether the result is NULL
    message("getting cached data")             #if not Null, displays message "getting cached data"
    return(m)                                  #prints the cached or calculated value of the inverse of the matrix
  }
  data <- x$get()                              #assigns 'data' to the retrieved input matrix x
  m <- solve(data, ...)                        #calulates the inverse of the 'data' matrix
  x$setsolve(m)                                #stores the inverse of the 'data' matrix in cache/memory
  m                                            #prints out the inverse of the 'data' matrix, called m         
}


##The following code tests whether the functions shown above work properly.  Uncomment to test and confirm that they work. 

# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# m1
# 
# myMatrix_object <- makeCacheMatrix(m1)
# 
# cacheSolve(myMatrix_object)
#


