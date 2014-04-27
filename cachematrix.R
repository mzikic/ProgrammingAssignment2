## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function creates a special kind
## of matrix, which is really a list consisting of
## 1. set function - used to set the values of matrix
## 2. get function - used to retrieve the values of matrix
## 3. setinverse - used to set inverse of matrix
## 4. getinverse - used to retrieve inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # set initialises value, including setting the inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # return x
    get <- function () x
    # set inverse i to values being passed in
    setinverse <- function(solve) i <<- solve
    # return i
    getinverse <- function() i
    # function return values
    list (set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}

## Write a short comment describing this function
##The following function calculates the mean of the special 
##"matrix" created with the above function.  The function checks
##to see whether the inverse has already been calculated
##and if so, it skips the calc and retrieves it from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get inverse if in cache
    i <- x$getinverse()
    ## if found in cache, return it
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## not in cache, so we need to calculate
    ## get data
    data <- x$get()
    ## solve data (get inverse matrix)
    i <- solve(data)
    ## update the cache for this vector with results
    x$setinverse(i)
    ## return results
    i
}
