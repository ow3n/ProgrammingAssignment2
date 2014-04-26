## A pair of functions to create and calculate the inverse of
## a matrix with caching ability.

## Construct a matrix with caching ability.
## Input: A matrix object created in R.
## Output: A list of 4 functions to:
##	1) Set the matrix to a new matrix.
##	2) Get the matrix.
##	3) Set the inverse of the matrix (to cache it for later).
##	4) Get the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix())
{
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Calculate the inverse of a matrix, using the cached value if
## it has already been calculated and the matrix hasn't changed.
## Input: A matrix that has been constructed with the above
##	'makeCacheMatrix' function.
## Output: The inverse of the specified matrix.
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	if (!is.null(inv))
	{
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}

