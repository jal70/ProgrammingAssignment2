
## Get a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # invX stores the inverse
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
    # new matrix set
  }
  # get new matrix
  get <- function() x
  # set the inverse
  setInv <- function(Inv) invX <<- Inv
  getInv <- function() invX
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of a matrix if it is not stored

cacheSolve <- function(x, ...) {
  # Check if inverse is stored
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    # Return the cached value
    return(invX)
  }
  data <- x$get() # ger the matrix
  invX <- solve(data, ...) # calculate the inverse
  x$setInv(invX) #Store the inverse in cache
  invX # Return computed inverse
}
