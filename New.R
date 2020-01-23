makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
              x<<-y
              m<<-NULL
        }
        get<-function() x
        setinverse <- function() invers <<- solve (x)
        getinverse <- function() invers
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        invers <- x$getInverse()
        if (!is.null(invers)) {
          message("getting cached data")
          return(invers)
        }
        matrx <- x$get()
        invers <- solve(matrx, ...)
        x$setInverse(invers)
        invers
}