## makeCacheMatrix 
# Instead of creating a matrix with a direct assignment and the 'matrix' command,
# makeCacheMatrix creates four methods for setting and 
# retrieving the value of a matrix and for
# setting and retrieving the inverse of a matrix 
# The set methods also create a cached copy of the matrix itself
# and of the inverse of the matrix
# Returns: list, contains four methods above

makeCacheMatrix <- function(x = numeric()) {
    # create a matrix and define a set of methods 
    # that become named attributes of that matrix
    
    # clear the cache
    s <- NULL
    setm <- function(y) {
        # set the value of a matrix
        
        # cache the matrix
        x <<- y
        # clear previous value for inverse stored in cache
        s <<- NULL
    }
    getm <- function() x
        # returns the matrix itself
    setinverse <- function(solve) s <<- solve
        # cache the value computed for the inverse of matrix
        # in this context, inverse is a variable and NOT a function
    
    getinverse <- function() s
    list(setm = setm, getm = getm,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
# calculates the inverse of a matrix using the get method of makeMatrix.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    # only computes the inverse when the cache is empty
    data <- x$getm()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}

mx <- matrix(c(1, 2, 3, 4), ncol = 2)
my <- makeCacheMatrix(mx)
my$getm()
my$getinverse()

cacheSolve(my)
