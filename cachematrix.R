## Matrix inversion is usually a costly computation. Cachematrix provides the ability to caching and inversing of a matrix.
## Two functions provided below cache the inverse of a matrix and computes the matrix.

#################################################################################################
## Function Name: makeCacheMatrix
## Description  : This function creates a matrix object and functions to get and set the matrix
##                and functions to get and set an inverse of the matrix.
#################################################################################################

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		
        set <- function( y ) {
                x <<- y
                m <<- NULL
        }
		
        get <- function() x
        
		setinverse <- function( inverse ) m <<- inverse
        
		getinverse <- function() m
        
		list( set = set, 
              get = get,
              setinverse = setinverse,
              getinverse = getinverse )
}


#################################################################################################
## Function Name: cacheSolve
## Description  : This function computes the inverse of the matrix returned by 
##                makeCacheMatrix above. If the inverse has already been calculated 
##                and the matrix has not changed ( i.e. the inverse has not reset to null), 
##                then the cachesolve should retrieve the inverse from the cache.
#################################################################################################


cacheSolve <- function(x, ...) {
       m <- x$getinverse()
	   
       if( !is.null( m ) ) {
               message( "getting cached data" )
               return( m )
       }
       
	   data <- x$get()
       m <- solve( data )
       x$setinverse( m )
	   
       return( m )
}

