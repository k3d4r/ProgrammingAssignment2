## x is regarded as a matrix
##solved value "s" is set as NULL
## solve is nothing but solving inverse
makeCacheMatrix <- function(x=matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}
 ## cacheSolves gives us the inverse of the matrix returned by the above function
cacheSolve <- function(x,...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s) 
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolve(s)
  s
}
             
