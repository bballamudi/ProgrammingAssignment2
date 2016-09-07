## Function makeCacheMatrix receives a matix and evaluates inverse of it and stores it for future reference. Computing 
## inverse of a matrix is expensive and time consuming so if taking inverse of a particular matrix is a repetative then
## saving this computational value and useing it makes more sense than having to solve it again and again. The other
## function cacheSolve atually gets called for a matix inverse computation and this function checks in makeCacheMatrix 
## function to see if the computation is already done and the result is saved from previous computation. If it finds the
## desired value computed and stored in makeCacheMatrix function then it simply pulls the value from it and return it to 
## the caller function. In case of non existing, cacheSolve funtion computes the inverse of a matrix and saves it to the
## makeCacheMatrix function for future use.

## As described this funtion doesn't compute the value by itself but stores the results of the computation of the 
## function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set,get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #x <- makeCacheMatrix(x)
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cache data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
