##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){ #set the matrix
	    x <<- y
	    m <<- NULL
	  }
	get <- function() x #get the matrix
	setInverse <- function(inverse) m <<- inverse #set the inverse of the matrix
	getInverse <- function() m #get the inverse of the matrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #return a list of them
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
	if(!is.null(m)) { #if its set, return the inverse 
	    message("getting cached data")
	    return(m)
	}
        data <- x$get()
	m <- solve(data)
	x$setInverse(m) #set the inverse
	m #return the matrix
}
