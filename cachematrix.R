## @Topic: Assignment2 for R programming Coursera Course
## @Date: 20th of February 2015
## The following functions calculate the inverse of a matrix, cache it 
## and retrieve it from cache if already calculated

## Create a matrix in order to cache its inverse when calculated 

makeCacheMatrix <- function(x = matrix()) {

	# Prepare the cache
	myinverse <- NULL
	set <- function(y){
		x <<- y
		myinverse <<- NULL
	}
	# Calculate the inverse 
	get <- function() x
	setinverse <- function(solve) myinverse <<- solve 
	getinverse <- function() myinverse

	# Define a list
	list(set= set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## Calculate the inverse of a matrix, first check if it is already
## in the cache, if yes, retrieve it from there, if no, calculate it 
## and put it in the cache

cacheSolve <- function(x, ...) 
{
	myinverse <- mymatrix$getinverse()
	
	# Check if it is already cached
	if (!is.null(myinverse))
	{
		message("getting data from cache")
		return(myinverse)
	}
	
	# Calculate the inverse of the matrix, it is only done
	# if not already cached
	mydata <- x$get()
	myinverse <- solve(mydata, ...)
	x$setinverse(myinverse)
	return(myinverse)
}

## To test the function comment out the last two lines
## Thanks to Scott von Kleeck for the test matrix - I used more time to find out a inversable matrix than for coding :-)

#mymatrix <- matrix(c(1, 3, 2, 4),nrow=2,ncol=2)
#solve(mymatrix)
