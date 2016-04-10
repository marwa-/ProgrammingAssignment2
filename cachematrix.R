## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makeCacheMatrix takes a matrix x 
## and returns a list of functions set, get, setinverse, getinverse

## It caches the matrix in set and enables you to get the matrix back 
## via function get.
## The inverse is supplied to be stored via setinverse 
##And the inverse of the matrix can be cached back through getinverse


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function()m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse)

}




## Write a short comment describing this function
## The cachSolve function finds the inverse of a square invertible matrix
## It first tries to find a stored value for this matrix
## If not available it calculates the inverse then 
## set it to be caches via setinverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
