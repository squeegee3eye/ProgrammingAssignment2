## Programming Assignment 2: Lexical Scoping
## R Programming, 27 July 2014
## Purpose of functions are to learn how lexical scoping works



## mx: variable to store value of matrix inverse


## Returns a list of four functions: set, get, setmxinverse, getmxinverse
## set: change matrix value and reset calculated inverse
## get: returns the matrix
## setmxinverse: set matrix inverse, save the inverse of the matrix for future use
## getmxinverse:  calculate the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  mx <- NULL
  
  ## change matrix value and reset calculated inverse
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## save the inverse of the matrix for future use
  setmxinverse <- function(mxinverse) mx <<- mxinverse
  
  ## calculate the inverse of the matrix
  getmxinverse <- function() mx
  
  ## return the list of functions
  list(set = set, get = get,
       setmxinverse = setmxinverse,
       getmxinverse = getmxinverse)
  
}


## Calculates the inverse of a matrix
## checks if it has already been calculated then uses the previoulsy calculated value if it exists
## if not, then calculate the inverse, saves it for later use and returns it.
## assumes the matrix is invertible

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
  
    ## check if the inverse has already been calculated and saved
    mx <- x$getmxinverse()
    if(!is.null(mx)) 
    {
        ## inverse exists, respond with message and return the inverse
        message("getting cached data")
        return(mx)
    }
  
    ## calculate the inverse 'mx', save inverse using 'setmxinvsere' and return the inverse
    data <- x$get()
    mx <- solve(data)
    x$setmxinverse(mx)
    return(mx)
  
}
