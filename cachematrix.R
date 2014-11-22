
# In this programming example the <<- operator is introduced which 
# can be used to assign a value to an object in an environment
# that is different from the current environment ("lexical scoping"). 

# Below are two functions that make use of lexical scoping in R.

# 1. "makeCacheMatrix()" is used to create a special object
# that stores a matrix and can cach its inverse.

makeCacheMatrix <- function(x = matrix()) {   # input x will be a matrix

  inv <- NULL   # inv will be our inverse and it is reset to NULL
                # every time makeCacheMatrix() is called
  
                # the next three functions are not run when makeCacheMatrix()
                # is called. Instead, they will be used by cacheSolve() to 
                # get values for x (our matrix) or for variable inv
                # (our inverse) and for setting the inverse. 
                # These functions are the 'methods' of the object
                # defined by makeCacheMatrix
  
  set <- function(y) {   # setter function -> takes an input matrix

    x <<- y              # saves the input matrix  

    inv <<- NULL         # resets the inverse to NULL as new object
                         # is created
  }
  get <- function() { x }    # getter function -> this function will
                             # retrurn the value of the original matrix
  
  setinverse <- function(my_inv) { inv <<- my_inv } #  
                         # this function is called by cacheSolve()
                         # during the first access by cacheSolve()
                         # and it will cache the inverse
                         # by using superassignment (<<-)
                         
  getinverse <- function() { inv } # this will return the cached inverse
                                   # to cacheSolve() on subsequent access
                                   # (...so we hit the goal for this programming example!)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
                 # with these internal functions, a new object of
                 # class 'list' is created. These procedures will be
                 # activated each time makeCacheMatrix() is called.
}


# 2. "cacheSolve" computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated
# (and the matrix has not changed), then the function cachesolve()
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  # the input is an object created by makeCacheMatrix
  
  inv <- x$getinverse()       # accesses the object x and gets the value of the inverse
  
  if(!is.null(inv)) {         # if inverse is not NULL (already cached)....
    
    message("getting cached data") #... this message is sent to the console
    
    return(inv)               # and the cached inverse will be returned 
                              # in this case we leave the function cacheSolve() 
  }
  data <- x$get()             # this part of the code is only reached when 
                              # x$getinverse does not return an inverse
                              
                              # in the next few steps we try to solve for the 
                              # inverse of x - tryCatch() will throw an error
                              # message in case the original matrix is singular
  tryCatch({              
    inv <- solve(data, ...)  # if inv were NULL then we have to calculate the inverse 
    
    x$setinverse(inv)        # and the inverse will be stored in the object
    
    inv},                    # return the inverse to the code that called the function
    
    error = function(e){return("Sorry guy! System seems to be exactly singular. no inverse found!")})  
}

