## Caching the Inverse of a Matrix


## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL                       #set m as NULL
	set <- function(y) {
	                                #assign a value to an object in an environment 
	                                #that is different from the current environment
		x <<- y
		m <<- NULL
	}
	get <- function() x             
	setInverse <- function(Inverse) m <<- Inverse   #save the Inverse to the cache
	getInverse <- function() m                      
	list(set = set, get = get,                      #output
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated, then 
## the cachesolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()             #query the x's cache
	if(!is.null(m)) {               #if there is a cache
		message("getting cached data") 
		return(m)               #just return the cache, no computation needed
	}
	data <- x$get()                 #if there's no cache
	m <- solve(data)                #R actually compute them here
	x$setInverse(m)                 #save the result back to x's cache
	m     
}
