makeCacheMatrix <- function(x = matrix()) { #Create Function
        m<-NULL                             #Create empty variable
        set<-function(y){
              x<<-y                         #Functoin get matrix in cache
              m<<-NULL
        }
        get<-function() x                   #assignment function from cache
        setinverse <- function() invers <<- solve (x)       #Create inverse function and assignment
        getinverse <- function() invers                     #Call inverse function from cache assignment
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)       # Name parametrs for calling
}

cacheSolve <- function(x, ...) {            #Create Function
        invers <- x$getInverse()            #Assignment inverse function
        if (!is.null(invers)) {
          message("getting cached data")    #Check our cache and get the invers matrix
          return(invers)
        }
        matrx <- x$get()                     
        invers <- solve(matrx, ...)         #The same procedure as under
        x$setInverse(invers)
        invers
}