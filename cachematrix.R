# makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

# cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix.
#             If the inverse has already been calculated (and the matrix has not changed), 
#             then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix :
#x: a square invertible matrix
# return: a list containing functions to
#              1. set the matrix
#              2. get the matrix
#              3. set the inverse
#              4. get the inverse
#         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  inv<- NULL
  set<- function(y) {
    
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 

    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv <<- inverse 
  getinverse<- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
  
}


##cacheSolve :
  # @x: output of makeCacheMatrix()
  # return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  

  
  inv = x$getinverse()
  
  # if the inverse has already been calculated :-
  
  if (!is.null(inv)){
  
      # gets it from the cache and skips the computation. 
    
    message("getting cached data")
    return(inv)
  }
  
  # else, calculates the inverse 
  
  data = x$get()
  inv = solve(data, ...)
  
  # sets the value of the inverse in the cache through the setinv function.
  
  x$setinverse(inv)
  
  return(inv)
  
  # Return a matrix that is the inverse of x
}

