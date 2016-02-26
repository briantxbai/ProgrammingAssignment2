## This files contains the second programming assignment for the R programming course

## In the makeCacheMatrix function, makeCacheMatrix creates a special "Matrix",
## which is a matrix that contains a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv_matrix <- NULL
	set <- function(y){
		x <<- y
		inv_matrix <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv_matrix <<- inverse
	getinverse <- function() inv_matrix
	list(set = set,
		      get = get,
		      setinverse = setinverse,
		      getinverse = getinverse)
}


## The following function calculates the inverse of the speical "matrix" created with the above
## function. It retrieves the inverse and skips the computation if the inverse has already been
## calculated. In contrast, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setmean function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$getinverse()
	if(!is.null(inv_matrix)){
		message("getting cached data")
		return(inv_matrix)
	}
	data <- x$get()
	inv_matrix <- solve(data,...)
	x$setinverse(inv_matrix)
	inv_matrix
}
