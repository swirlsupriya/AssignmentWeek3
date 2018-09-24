## This function takes input as a matrix and gives out a special vector 
## Vector has attributes which sets/gets a matrix and set(get) the inverse of matrix

## This function sets the information of a matrix and later on checks the global environment for the cache

  makeCacheMatrix<- function(x=matrix())
  {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix=mat) inverse <<- inverse_matrix
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  




## This function returns the inverse of matrix if present in cache else calculates inverse and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix <- x$getinverse()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data)
  x$setinverse(matrix)
  message("newly calculated")
  matrix
}
