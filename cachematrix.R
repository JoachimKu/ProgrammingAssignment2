##----------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    ##
    ## Creates a special "vector", which is a list containing a function to
    ##
    ##  sets the matrix to cache
    ##  gets the matrix from cache
    ##
    ##  sets the inverted matrix to cache
    ##  gets the inverted matrix from cache
    ##
 
    invmat <- NULL       # clears the invmatrix
    
    ##############################
    # setmatrix
    # sets the matrix to the cache
    # clears the invmatrix
    #
    setmatrix <- function(mat) {
        x <<- mat
        invmat <<- NULL
    }
    
    ##############################
    # getmatrix
    # returns the chached matrix
    #
    getmatrix <- function() x

    ##############################
    # setinvmatrix
    # sets the (inverse)matrix to the cache
    #
    setinvmatrix <- function(imat){
        invmat <<- imat     
    } 
    
    ##############################
    # getmatrix
    # returns the cached (inverse)matrix
    #
    getinvmatrix <- function() {
        invmat
    }

    ##############################
    # lists all     
    #
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


##----------------------------------------------------------
## cacheSolve
##
## Returns a matrix that is the inverse of 'x' 
## if the inv matrix is in the cache, no new calc is neeeded
##
cacheSolve <- function(x, ...) {
  
    im <- x$getinvmatrix()
    if(!is.null(im)) {                  # checks, if inverse matrix exists
        message("getting cached data")  # some info
        return(im)                      # returns the cached data
    }
    
    data <- x$getmatrix()    # otherwise, it takes the cached matrix and
    im <- solve(data, ...)   # compute the inverse matrix
    x$setinvmatrix(im)       # sets the inverse matrix to the cache
    im                       # returns the inverse matrix
}