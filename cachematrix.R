## The first function makeCacheMatrix sets a initial matrix 'x' as w in the cache
## The four functions in this are:
## 1 - set - assigns values in the parent environment for x as the initial matrix and w as null
## 2 - get - retrieves x from the parent environment of makeCacheMatrix
## 3 - setmatrix - assigns the input value to w
## 4 - getmatrix - defines the getter of the setmatrix function as w 


makeCacheMatrix <- function(x = matrix()){
  w <- NULL
  set <- function(y) {
    x <<- y
    w <<- NULL
  }
  get <- function() x
  setmatrix <- function(newmat) w <<- newmat
  getmatrix <- function() w
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## CacheSolve returns the inverse of the x matrix
## first it calls the getmatrix function from makeCacheMatrix and sets that to w
## next, it checks to see if w is not null (matrix is properly set in the cache)
## if that is present, the message 'getting cached data' is returned
## then the x matrix is retrieved using the get function (stored as data in this function)
## next, the inversion of data is taken and set as 'w'
## lastly w is printed

cacheSolve <- function(x, ...) {
  w <- x$getmatrix()
  if(!is.null(w)) {
    message("getting cached data")
    return(w)
  }
  data <- x$get()
  w <- solve(data)
  x$setmatrix(w)
  w
}

