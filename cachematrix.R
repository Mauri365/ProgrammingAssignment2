##Matrix inversion is usually a costly computation and there may be some 
##benefit to caching the inverse of a matrix rather than compute it repeatedly .

##This function creates a special “matrix” object that 
##can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #a variable is created to store the inverse of the matrix x
  
  ##function to change the cached matrix when a new matrix is entered
  set <- function(y) {
    x <<- y    #store the new matrix in the cache
    m <<- NULL #resets the inverse cache (set null) when a new matrix is entered
  }
  
  #Get back the matrix in the cache
  get <- function() x
  
  #assign the compute inverse matrix, to the cached inverse matrix
  setinverse <- function(inverse) m <<- inverse
  
  #Get back the cached inverse matrix
  getinverse <- function() m
  
  #create a list of names for functions (makes their calls easier)
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

##cacheSolve: This function computes the inverse of the special “matrix” 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache. 
##The inverse of the matrix is calculated  with the "solve" function.
cacheSolve <- function(x, ...) {
      
  
  m <- x$getinverse()
  
  #if the inverse matrix in the cache is NOT NULL retrieve the inverse matrix and return the value. 
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if the inverse matrix is NULL calculates de inverse using the solve() function
  data <- x$get() #get back the matrix in the cache 
  m <- solve(data, ...) # calculates the inverse
  x$setinverse(m) #store the inverse  matrix  the cache
  
  m # Return and displays the inverse matrix.
}