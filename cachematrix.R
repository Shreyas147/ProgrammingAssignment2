## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  {
    j <- NULL                                           ##NULL assigned to J
    set <- function(y)                                  ##setting value of matrix 
      {                                                 
      x <<- y                                           ## <<- assigns a value to an object in an environment                                                   
      j <<- NULL                                        ## that is different from the current environment                                     
      }
    get <- function()x                                  ##getting value of matrix
    setInverse <- function(inverse) j <<- inverse       ##Setting value of inverse
    getInverse <- function() j                          ##getting value of the inverse
    list(set = set, get = get, 
    setInverse = setInverse, 
    getInverse = getInverse)
  }

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
  {
    j <- x$getInverse()                                ##returns a matrix that is inverse of x and assigned to j
    if(!is.null(j))                                    ## if inverse of x is already calculated,it can get the 
      {                                               ## inverse from the cache
        message("getting cached data")
        return(j)
      }
    mat <- x$get()                                    
    j <- solve(mat,...)                                 ##for computing inverse of matrix we will use solve()function
    x$setInverse(j)                                     ##setting the value of inverse of matrix
    j
}
