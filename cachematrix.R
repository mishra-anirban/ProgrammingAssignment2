## The folwing funtions compute the inverse of a matrix
## Since this is a costly computation, once computed the 
## inverse is cached and henceforth retrived from the cache

## The makeCacheMatrix function creates a special matrix object
## that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y)
        {
                inv <<- NULL
                x <<- y
               
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getInverse()
        ## Check if already calculated
        if(!is.null(inv_x))
        {
                ## Fetch from Cache
                message("Getting Cached Data")
                return(inv_x)
        }
        ## Retrive matrix, compute inverse and store in cache
        x_val<-x$get()
        inv_x <- solve(x_val)
        x$setInverse(inv_x)
        inv_x
}
