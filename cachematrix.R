## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

cacheTest <- function()
{
    Y <- matrix(c(3,3.2,3.5,3.6),2,2)
    cache<-makeCacheMatrix()
    cache$set(Y)
    iY <- cacheSolve(cache)
    
## The inverse of Y is Y^-1 only when:
## Y x Y^-1 = I

    ans <- Y %*% iY
    ans
    
}
