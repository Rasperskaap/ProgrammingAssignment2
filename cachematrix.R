# A pair of functions that cache the inverse of a matrix
#CREATE SPECIAL MATRIX OBJECT THAT CAN CACHE ITS INVERSE
makeCacheMatrix <- function( m = matrix() ) {
    #INITIALIZE INVERSE PROPERTY
    i <- NULL
    #SET THE MATRIX
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    #GET THE MATRIX
    get <- function() {
    	#RETURN THE MATRIX
    	m
    }
    #SET THE INVERSE OF THE MATRIX
    setInverse <- function(inverse) {
        i <<- inverse
    }
    #GET THE INVERSE OF THE MATRIX
    getInverse <- function() {
        #RETURN THE INVERSE
        i
    }
    #RETURN THE LIST OF METHODS
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
#COMPUTE THE INVERSE OF SPECIAL MATRIX RETURNED BY "MAKECACHEMATRIX"
#IF INVERSE FOUND AND MATRIX REMAINED UNCHANGED "CACHESOLVE" WILL GET INVERSE FROM CACHE
cacheSolve <- function(x, ...) {
    #RETURN INVERSE MATRIX OF X
    m <- x$getInverse()
    #JUST RETURN INVERSE IF ALREADY SET
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    #GET MATRIX FROM OBJECT
    data <- x$get()
    #CALCULATE INVERSE USING MATRIX MULTIPLICATION
    m <- solve(data) %*% data
    #SET INVERSE TO OBJECT
    x$setInverse(m)
    #RETURN THE MATRIX
    m
} 
