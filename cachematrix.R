## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y){
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
     
    print(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  data = matrix()
        ## Return a matrix that is the inverse of 'x'
  
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




