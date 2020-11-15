## Two functions used to compute and cache the inverse of a matrix
## Especially useful for caching inverse of large matrix. 

#' Function that sets the matrix and the inverse in an environment
#' x input is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) m <<- invmat
  getinvmat <- function() m
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


#' Compute and cache the inverse of a matrix
#' Input x the result of makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmat(m)
  m
}
