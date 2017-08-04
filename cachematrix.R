makeCacheMatrix <- function(x=matrix()){
				#	x is a square invertible matrix
	inv <- NULL		#	inv wil contain the inverse of the matrix
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv) 
				#	returns a list containing function to 
				#		1. set the matrix
				#		2. get the matrix
				#		3. set the inverse
				#		4. get the inverse
}

cacheSolve <- function(x,...){
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	return(inv)
				#	returns inverse of the original matrix input to makeCacheMatrix()
}