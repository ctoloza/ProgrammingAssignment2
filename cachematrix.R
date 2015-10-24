## These functions can be used to calculate the inverse of a matrix and
## store it, allowing to get previous calculations, avoiding unnecessary recalculations

## This function is a constructor function to control the getters and setters

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {#used to set the values of the matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function() x # used to get the matrix (when exists)
  
  #setters and getters
  setinversa <- function(inversa) m <<- inversa
  getinversa <- function() m
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}


## This function calculates the inverse matrix unless the inverse matrix was previously calculated
## In this case, the function returns the inverse matrix previously calculated

cacheSolve <- function(x, ...) {
  # verifies if the inverse already exists.  In this case
  # the function returns the inverse already calculated
  m <- x$getinversa()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the inverse does not exists, then the inverse is calculated, returned
  # and stored in the m variable
  data <- x$get()
  m <- solve(data, ...)
  x$setinversa(m)
  m
}