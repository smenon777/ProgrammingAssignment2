## Put comments here that give an overall description of what your
## functions do
## These functions creata a cached version of solve that will cache the inverse 
## of a matrix if it is not cached and will return the previously cached version of the inverse
## if that is there

## This function creats a list of functions and and internal environment that will 
## enable the caching of inverses of a matrix

makeCacheMatrix <- function(x = matrix()) {
   invm <- NULL
   set <- function(y) {
    x <<- y
    invm <<- NULL
   }
  
   get <- function() x
   setInv <- function(Inv) invm <<- Inv
   getInv <- function() invm
   list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## This method will take the list above and use it to cache and retrive from the cache the 
## the inverse of a matrix that is passed in.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i<-x$getInv()
		if (!is.null(i)) {
		   message("getting cached data")
		   return(i)
		}
		
		i <- solve(x$get())
	    x$setInv(i)
		i
}
