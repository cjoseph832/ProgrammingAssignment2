## function creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #setting inverse value to be null to begin
  m<-NULL
  set<-function(y){
    #assigning these values in the environment function is defined (not locally)
    x<<-y
    m<<-NULL
  }
  get<-function() x
  #function to set the inverse value 
  setinverse<-function(solve) m<<-solve
  #function to retrieve the inverse value that was set in the function above 
  getinverse<-function()m
  #designates output list
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## computes inverse of the special matrix returned by the function above (if inverse has already been solved for, this function retrieves the inverse from the cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #attempts to retrieve inverse that has already been solved for 
  m<-x$getinverse()
  #testing if the inverse value is null or not...if is not null, then m is returned (inverse retrieved from original function)
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  #retrieves matrix from the original function that the inverse is to be calculated on 
  data<-x$get()
  #calculates inverse of matrix
  m<-solve(data)
  #sets m to be the inverse of the matrix and what is to be outputted
  x$setinverse(m)
  m
}

