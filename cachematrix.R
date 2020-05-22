## Creating a function to inverse a matrix

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse



##
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-null
        }
        get<-function() x
        setInverse <- function(solve) m<<-solve
        getInverse <- function() m
        list(set = set,get = get,getInverse = getInverse,setInverse=setInverse)
}


## The following function calculates the inverse of the special "Matrix" 
##created with the above function. However, it first checks to see 
##if the Inverse has already been calculated. If so, it gets the Inverse from 
##the cache and skips the computation. Otherwise, 
##it calculates the Inverse of the data and sets the value of the Inverse in the 
##cache via the setInverse function.

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)){
                message("Your inverse is")
                return(m)
        }
        data <-x$get()
        m<- solve(data,...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
