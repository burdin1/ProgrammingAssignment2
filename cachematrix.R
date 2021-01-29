## makeCacheMatrix creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x=matrix()){
    m<-NULL
    set<-function(y){
    x<<-y
    m<<-NULL
    }
    get<-function()x
    setinv<-function(solve) m<<-solve
    getinv<-function()m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    }


## cacheSolve computes the inverse of the special matrix returned by the function above. If the inverse has been already computed, then the cacheSolve retrieves the inverse from the cache producing a message

cacheSolve <- function(x, ...){
    m<-x$getinv()
    if(!is.null(m)){
    message('getting that cached data yo')
    return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinv(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
