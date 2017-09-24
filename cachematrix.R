## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## First, setmat sets the matrix'value
## Next, getmat gets the value of the matrix
## then, setinv sets the inverse of the matrix
## and getinv the inverse value


makeCacheMatrix <- function(x = matrix()) {

invmat=NULL

setmat<-function(y){
        x<<-y
        invmat<<-NULL
}
getmat<-function()x
setinv<-function(inverse)invmat<<-inverse
getinv<-function()invmat
list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
##cachesolve uses the matrix and checks value
## if it has value than it return the message
## if it is empty than it returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        
        if (!is.null(invmat)) {
                message ("get cached inversed matrix")
                return (invmat)
        }
        
        matdat<-x$getmat()
        invmat<-solve(matdat, ...)
        
        x$setinv(invmat)
        
        return(invmat)
        
}
