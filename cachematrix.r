## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	# Initially set to NULL
	# Changes when the user sets the value
    inv <- NULL

    # set function
    # Sets the matrix itself but not the inverse
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    # get function
    # Gets the matrix itself but not the inverse
    get <- function() x

    # Manually set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # Get the inverse
    getinverse <- function() inv

    # Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  or cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   		 m <- x$getInverse()
    
   	  ## Just return the inverse if its already set
 		   if( !is.null(m) ) {
    		   message("getting cached data")
        	   return(m)
   		   }
    
  	  ## Get the matrix from our object
 		   data <- x$get()
 	 		  
 	  ## Calculate the inverse using matrix multiplication
    		   m <- solve(data) %*% data
    
   	  ## Set the inverse to the object
    		   x$setInverse(m)
    
    	  ## Return the matrix
    			m
}