## Two functions makeCacheMatrix and cacheSolve are defined as below
## The makeCacheMatrix defines an matrix type object with 4 different operations: set, get, setinverse and getinverse

## see line by line comments

makeCacheMatrix <- function(x = matrix()) {
  ## Two values are stored in the object (1) matrix value x (2) inverse matrix value mInverse
  mInverse<-NULL ## initialize the object, set inverse as NULL
  set<-function(mInput){
    x<<-mInput ## assign the matrix value to the defining environment
    mInverse<<-NULL ## initialize the object, set inverse as NULL
  }
  get<-function(){
    x ## return object value
  }
  setinverse<-function(mInputInverse){
    mInverse<<-mInputInverse ## assign the inverse matrix to the defining invironment
  }
  getinverse<-function(){
    mInverse ## return inverse matrix value
  }
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse ## the return value of function mkeCacheMatrix is a list of 4 functions
    )
}


## see line by line comments

cacheSolve <- function(x, ...) {
  mSolution<-x$getinverse() ## get inverse value from the object
  if( !is.null(mSolution) ){ ## check if the inverse has been calculated before
    message("getting cached data") ## if yes, display message
    return(mSolution) ## inverse has been calculated before, return the cached value
  }
  ## when the inverse has not been calculated before
  data<-x$get() ## obtain the matrix value of the object
  mSolution<-solve(data,...) ## calculate inverse, use ... operator to pass additional setting for inverse calculation
  x$setinverse(mSolution) ## set and save the inverse matrix value into the object
  mSolution ## return the inverse matrix value
}
