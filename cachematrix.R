## A pair of functions to cache the inverse of a large matrix

## This function creates a list of functions that allow to set the matrix (set),
## get it (get), set it's inverse (setinv) and get it (getinv).

makeCacheMatrix <- function(x = matrix()) {
      
      I <- NULL
      
      set <- function(y){
            x <<- y
            I <<- NULL
      }
      
      get <- function() x
      
      setinv <- function(y) I <<- y
      
      getinv <- function() I
      
      list(st = set, get = get, setinv = setinv, getinv = getinv)
      
}


## This functions checks if the inverse was already calculated. If it was then
##it returns it, otherwise it calculates it first, and then returns it.

cacheSolve <- function(x, ...) {
      
      I <- x$getinv()
      
      if (!is.null(I)){            
            return I
      }
      
      mat <- x$get()
      I <- solve(mat, ...)
      x$setinv(I)
      
      I
}
