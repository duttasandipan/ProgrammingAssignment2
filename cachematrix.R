"This is the main block for the code and makes appropriate function calls"

a <- matrix(rnorm(9),nrow=3) # Please choose any random invertible matrix as you wish!!
my_matrix <- makeCacheMatrix(a)
my_matrix$setinverse(cacheSolve(my_matrix))
cacheSolve(my_matrix)

"This function initializes a new matrix with 4 associated functions:
1. set - Re-initializes the matrix to a new one and erases the cached inverse matrix
2. get - Fetches the matrix (if initialized already)
3. setinverse - Caches the inverse of the matrix in the variable inv
4. getinverse - Returns the cached matrix inverse (if available)"

makeCacheMatrix <- function(x=matrix()){
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

"This function calculates the inverse of the matrix if it isn't already stored in the 
cache. If the inverse is already stored in the cache, it returns the cached value"

cacheSolve <- function(x, ...){
  
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}