## This assignment illustrates how R works with scoping
## The overall goal of the functions is firstly to define
## a matrix and store the object, to cache the matrix and its inverse. 
## The second function allows to retrieve the inverse of the
## matrix stored in the makeCacheMatrix environment. 



## makeCacheMatrix defines and stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL             ##object inv initialized NULL
            set <- function(a){     ## Set the value of x and inv.
                    x<-a
                    inv<-NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv<<-inverse ##Setmean assing the input inverse to inv
            getinverse <- function () inv
            list(set=set,           ## return the elements
                 get=get,
                 setinverse=setinverse,
                 getinverse=getinverse)

}


## cacheSolve is necessary to retrieve the inverse 
## from the 'special' cached matrix defined in makeCacheMatrix()

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()   ## Return a matrix that is the inverse of 'x'
            if (!is.null(inv)){     ## if inv is not NULL, the cache inverse can be returned
                    message("Getting Cached Data")
                    return(inv)
            }                       ## if inv is NULL, then cacheSolve gets the matrix 
            data <-x$get()          ## from the input x and determines the inverse.
            inv <- solve(data,...)
            x$setinverse(inv)
            inv                     ## print the inverse
            
}
