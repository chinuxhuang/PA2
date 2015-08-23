## The goal is to make an inverse matrix cached for future invoke.

## Make an inverse matrix stored it in an enviroment vairable.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## To solve the matrix when it was not cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  data <- x$get()
  im <- solve(data,...)
  x$setinverse(im)
  im
}
