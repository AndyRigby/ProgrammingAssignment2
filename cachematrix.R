## These functions are used to calculate the inverse of a matrix, but
## since calculation of inverses can be computationally intensive, we
## also cache the result so that subsequent operations can access the result 
## rather than recalculating

## Write a short comment describing this function
## Creates a 'matrix' object holding the functions required to get and set the matrix to be 
## inverted as well as the inversion of that matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) m<<-inverse
  getInverse<-function()m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function inverts the matrix, but will first check whether this has already been
## solved. If it has then the previous result will be used and no calculations will be 
## performed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data<-x$get()  
    m<-solve(data)
    x$setInverse(m)
    m

}
