

#creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  #assigns the inverse property
  inv <- NULL
  
  #sets the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  #obtains the matrix
  get <- function() x
  
  #inverts the matrix
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  
  #obtains the inverted matrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


##computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  #if inverse is calculated, return it from cache
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #obtain the matrix
  data <- x$get()
  
  #computing the inverse of a square matrix
  inv <- solve(data)
  
  #inverse setting of the object
  x$setInverse(inv)
  
  #returning the matrix
  inv      
  
}
