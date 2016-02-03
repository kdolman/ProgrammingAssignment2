Create a list containing instructions for setting and getting the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
                  x <<- y
                  i <<- NULL
          }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check for a stored value of the inverse of matrix 'x', and if not available, calculate inverse

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if(!is.null(i)) {
          message("getting cached data")
          return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          return(i)
}
