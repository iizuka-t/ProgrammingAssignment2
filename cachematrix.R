## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly. Hereby, a pair of functions that cache the 
## inverse of a matrix has been written.

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
		s   <- NULL
		set <- function(y) {
		    x <<- y
		    s <<- NULL
		}
		get <- function() x
		setsolved <- function(solved) s <<- solved
		getsolved <- function() s
		list(set = set, get = get,
		     setsolved = setsolved,
		     getsolved = getsolved) 
}


##`cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` 
## above. If the inverse has already been calculated and the matrix has not changed, then `cacheSolve` 
## just retrieves the inverse from the cache.  If this is not the case, 
## it computes the inverse of the given matrix anew.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolved()
	if(!is.null(s)){
		message("getting cached data")
		return(s)
    	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolved(s)
	s
}

## Example
##> a <- matrix(1:4,2,2)  # 2 x 2 matrix
##> a
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## 

##> w <- makeCacheMatrix(a)
##> cacheSolve(w)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## 
##> cacheSolve(w)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 
##> b <-cacheSolve(w)
##getting cached data
##> 
##> a %*% b
##     [,1] [,2]
##[1,]    1    0
##[2,]    0    1
##> 
