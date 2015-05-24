## makeCacheMatrix is a function that contains a list of 4 functions:
	## 	1. set the matrix
	##	2. get the matrix
	##    3. set the inverse
	##	4. get the inverse
## It creates a "matrix" that caches its own inverse.

makeCacheMatrix <- function(x = Matrix()) {
  inv <- NULL
  set <- function(y) { 	# set changes the inverse stored in the main function by
    x <<- y # assigning the value of x to y.
    inv <<- NULL # Since value of matrix has changed, it assignes NULL to inver.
  }
  get <- function() x # Returns y which is the matrix stored in the function
  setinv <- function(inverse) { # Stores the the input value to inver
  inv <<- inverse  
  }
  getinv <- function(){ # This returns inver
  inv
  } 
  list(set=set, get=get, setinv=setinv, getinv=getinv) # 'makeCacheMatrix' returns a list object,
  }                                                    # which contains the 4 functions defined in the function 'makeCachematrix'.   

## cacheSolve calculates the inverse of the matrix created with the makeCacheMatrix function.
## If the inverse has been calculated, it gets the inverse from the cache and skips the computation phase.
## And if it hasn't been calculated, it calculates the inverse of the data, 
## and returns value of the inverse to the cache. 

cacheSolve <- function(x, ...) { 
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
