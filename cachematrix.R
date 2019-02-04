"The makeCacheMatrix  function takes a matrix as an argument in the form eg. makeCacheMatrix(matrix(c(5,1,8,2), nrow = 2, ncol = 2))
It creates a list with 4 functions:
1) set() : sets the input matrix
2) get() : gets the input matrix
3) set_invrse() : computes the inverse of the input matrix 
4) get_invrse() : returns the inverse of the input matrix, after solving it with set_invrse()
"

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x  #x is the matrix that is the input argument to the makeCacheMatrix function
  set_invrse <- function(x) i <<- solve(x) #solve(x) computes the inverse of the matrix x. Call set_invrse with a matrix argument as follows: eg. mk_Mtrx$set_invrse(matrix(c(5,1,8,2), nrow = 2, ncol = 2)) 
  get_invrse <- function() i #Need to set inverse with set_invrse() before you can run get_invrse() and return the inverse
  list(set = set, get = get,
       set_invrse = set_invrse,
       get_invrse = get_invrse) 
}

"
Checks cache for inverse and returns the inverse with a message if it's in the cache.
Otherwise it computes the inverse and returns it without a message.

Running this example code will correctly return the inverse from the cache:

#Call the makeCacheMatrix function with an input matrix and store it in an object named mk_Mtrx
mk_Mtrx<-makeCacheMatrix(matrix(c(5,1,8,2), nrow = 2, ncol = 2))

#Set the inverse of the input matrix
mk_Mtrx$set_invrse(matrix(c(5,1,8,2), nrow = 2, ncol = 2))

#Calling the cacheSolve function with mk_Mtrx as the input argument correctly returns the cached inverse
cacheSolve(mk_Mtrx)
"

cacheSolve <- function(x, ...) {
  i <- x$get_invrse()
  if(!is.null(i)) {
    message("getting cached inverse data") 
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) #Computes the inverse of the matrix and returns it if it's not in the cache (the message getting cached inverse data will be absent)
  x$set_invrse(i)
  i
}
