## This assignment has two functions: 
## makeCacheMatrix and cacheSolve
## Both functions are quite similar to makeVector and cachemean in the sample codes

## makeCacheMatrix: input must be a matrix - e.g., matrix(1:4,2)
## use set and get to retrieve the matrix
## use setInv and getInverse to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: check if inverse matrix is already in the cache. 
## See the if(...) part.
## the solve() function does the inverse of matrix
## x$setInv - set the inverse of value from x and using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}

## sample run example -----------------
## a <- makeCacheMatrix(matrix(1:4,2))
## a$get()
## a$getInverse()
## a$set(matrix(5:8,2))
## a$get()
## cacheSolve(a)
## cacheSolve(a)
## a$getInverse()
## b = a$getInverse()
## a$get() %*% b
