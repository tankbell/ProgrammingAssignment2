## The following two functions computes the inverse of
## a square invertible matrix and caches the inverse.
## Computing the matrix inversion is an expensive
## operation , a caching mechanism avoids recomputing
## the inverse of a matrix each time.

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse. It creates a list
## containing functions to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
## It takes as argument a square invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse
  inverse <- NULL

  ## function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  ## function to get the value of the matrix
  get <- function() x

  ## function to cache the inverse of the matrix
  ## <<- is used to assign a value to an object in an
  ## environment that is different from the current
  ## environment
  setinverse <- function(i) inverse <<- i

  ## function to get the value of the matrix inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If 
## the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## Check if the inverse has already been
  ## calculated
  if (!is.null(inv)) {
    ## If the inverse has already been calculated
    ## get the result from the cache and return
    ## the inverse without computing it again
    message("getting cached data")
    return (inv)
  }
  ## If the inverse is not already computed,
  ## compute the inverse and cache it using
  ## the setinverse function
  
  ## get the matrix
  data <- x$get()
  
  ## compute the inverse using the
  ## solve method
  inv <- solve(data, ...)
  
  ## cache the inverse
  x$setinverse(inv)
  
  ## Return the matrix that is the inverse of 'x'
  return (inv)
}

## Test case that compares the time taken
## to compute the inverse vs using the results
## from the cache
timeDiff <- function() {
  ## Random Number Generator
  r <- rnorm(1000000)
  ## Create a matrix with 1000 rows
  ## and 1000 columns
  m <- matrix(r, 1000, 1000)
  ## Call makeCacheMatrix with matrix m
  ## created above as the argument
  mv <- makeCacheMatrix(m)
  ## Note the start time
  startTime <- Sys.time()
  ## Compute the matrix inverse
  cacheSolve(mv)
  ## Find the total time taken to 
  ## compute the inverse
  totalTime <- Sys.time() - startTime
  print(totalTime)
  ## Call cacheSolve on mv again
  ## This time it will use the
  ## cached copy. Find the total
  ## time taken to finish this
  ## operation
  startTime <- Sys.time()
  cacheSolve(mv)
  totalTime <- Sys.time() - startTime
  print(totalTime)
}

## A sample run of the timeDiff function above is
## as follows :

## > timeDiff()
## Time difference of 1.979494 secs
## getting cached data
## Time difference of 0.0002071857 secs

## As we can see , by using the cached data
## we see a marked improvement in the time
## taken to get the inverse of a 1000 x 1000
## matrix

## Listed below are a few more sample runs
## of the makeCacheMatrix and cacheSolve functions

##> x <- matrix(1:4, 2, 2)
##> y <- makeCacheMatrix(x)

##> y$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

##> cacheSolve(y)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

##> cacheSolve(y)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
