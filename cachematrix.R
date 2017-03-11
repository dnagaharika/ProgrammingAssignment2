## This file contains two functions that calucate and retrieve the inverse of 
## a matrix using cache.

## the function "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m<-NULL
	setMat<-function(y){
		x<<-y
		m<<-NULL
	}
	getMat<-function() x
	setInv<-function(inverse){
		m<<-inverse
	}
	getInv<-function() m
      list(setMat = setMat, getMat = getMat,
             setInv = setInv,
             getInv = getInv)
}


## The function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getInv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$getMat()
	m<-solve(data,...)
	x$setInv(m)
	m
}
