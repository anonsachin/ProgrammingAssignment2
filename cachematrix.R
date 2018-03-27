
## SETTING THE MATRIX CACHING FUNCTION

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL ## INVERSE VARIABLE
  set <- function(y){
    x<<-y
    i<<- NULL
  }
  get <- function() x
  setinv <- function (INV) i<<- INV ## SETTING INVERSE
  getinv <- function () i
  list(set = set, get = get,
       getinverse = getinv,
       setinverse = setinv)
}


## TO SET THE VALUE OF THE MATRIX

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    l<- x$getinverse() ##FETCHING THE CACHED VARIABLE
    if(!is.null(l)){ ## CHECKING IF IT HAS BEEN CALCULATED
      message("RETURNING CACHE")
      return(l)
    }
    d<- x$get()
    l <- solve(d)
    x$setinverse(l)
    l
    
}
