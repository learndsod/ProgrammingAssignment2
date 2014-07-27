##Assignment2 to cache Inverse of a matrix and recompute inverse only if it has not been calculated already or the values have changed


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
##check if the input is a mtrix
  if(!is.matrix(x)) 
  {
    stop("Input must be a matrix")
  }
  
  ##get number of columns for the matrix for square matrix the rows will be the same
  nc <- ncol(x)        	
  
  ##initialize the values of original and inverse matrix
  m <- matrix(NA,ncol=nc, nrow=nc)
  z <- matrix(NA,ncol=nc, nrow=nc)
  
  
  
  set <- function(y) 
  {
    if(!is.matrix(y)) 
    {
      stop("Input must be a matrix")
    }
    
    x <<- y                	
    
    n <<- ncol(x)

  }        	
  
  get <- function() 
  {
    x
  }	
  
  
  setinverse <- function(matinv,matnew) 
  {
    message("Caching value of inverse and original matrix")
    m <<- matinv
    z <<- matnew
    
  }
  
  
  gets <- function() 
  {
    z
  }
  
  
  getinverse <- function() 
  {
    m
  }	        
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,gets = gets
  )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 z <- x$gets()
  data <- x$get()
  m <- x$getinverse()
  
  ##Check the new input versus the original matrix that was cahced
  ##If the original 
  if(identical(data, z)) 
  {
    message("getting cached data")
    return(m)
  }               
  
  
  m 	<- solve(data)
  
  z <- data        
  x$setinverse(m,z)
  
  m        
        
}
