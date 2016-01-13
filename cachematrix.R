## These functions allow caching of a matrix inverse.

## This function is a wrapper function for the matrix object. 
makeCacheMatrix <- function (x = matrix()) {
        i <- NULL
        #set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #get the matrix value
        get <- function() x
        
        #setInverse
        setInverse <- function(inverse) i <<- inverse
        
        #getInverse
        getInverse <- function() i
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## This function checks to see if the inverse of the CacheMatrix
## is computed already and if not it will compute the inverse. 
cacheSolve <- function(x, ...) {
        #retrieve inverse
		i <- x$getInverse()
		
		#check to see if the inverse is calculated if yes return
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
		
		#if the inverse is not calculated, calculate it and 
		#return
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}