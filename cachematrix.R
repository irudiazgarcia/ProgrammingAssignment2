## The functions "makeCacheMatrix" and "cacheSolve" are dedicated to create an object dedicated to store two matrices: the main one and its inverse, 
## which is calculated once the subfunction "setinverse" is called.

## makeCacheMatrix constructs the object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y){
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
     
 ## Useful as a debugging tool
    print(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## cacheSolve uses the parameters of the object to, first, make sure that the inverse tool has been used before, and
## prints the cached data or creates the inverse of the matrix by using the solve() function

cacheSolve <- function(x, ...) {
  data = matrix()
  
## "all" makes sure the whole matrix is "na"
      if(!(all(is.na(x$getinverse())))){
        print("getting cached data")
        return(x$getinverse())
      }
    
    else{
      inv = solve(x$get(), ...)  
      x$setinverse(inv)   
      inv    
    }      
    
}




