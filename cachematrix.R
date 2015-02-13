
## Definition of the matrix object which cache capabilities

makeCacheMatrix  <- function(x = matrix()) {

        # inverse matrix attribute
        inv <- NULL
        
        # matrix setter
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        
        # matrix getter
        get <- function() x
        
        
        # inverse get and set methods
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        
        # required to operate with object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Function return inverse matrix given cachable matrix object

cacheSolve <- function(x, ...) {
        
        
        # try to get result from cache
        inv <- x$getinverse()
        
        
        # if true means cache not empty thus retrun the value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
                
        # if you get here then cache was empty thus 
        # set cache and return the result 
        
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
        inv



}
