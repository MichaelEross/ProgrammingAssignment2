## The below functions are used to create a special object
## that stores a matrix and caches its inverse.  This allows 
## the function to return a stored inverse subsequently without
## performing an intensive inverse operation

## The makeCacheMatrix functions creates an object
## that is a list containing a function that will allow
##     1. Setting the value of a matrix
##     2. Retrieving the value of the matrix
##     3. Setting the inverse of the matrix
##     4. Retrieving the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## m within the created object stores values    
    m <- NULL
    ## create a function that assigns the matrix
    ## to x and clears m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## function to return the matrix
    get <- function() x
    ## function to set the inverse of the matrix
    ## to m which will be available from other envs
    setinv <- function(solve) m <<- solve
    ## function to return the stored inverse of m
    ## if it exists
    getinv <- function() m
    ## generate the object (target) of the function
    ## with a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}


## The cacheSolve function operates on an object created
## from the makeCacheMatrix function.  If that object
## does not contain a stored inverse, it will perform the 
## operation and store it within the object.  If the 
## object contains the inverse, a message is displayed that
## it is being retrieved, and the stored inverse is returned
## without recalculation

cacheSolve <- function(x, ...) {
    ## retrieve the value of m from the object which
    ## may or may not have a pre-calculated inverse
    m <- x$getinv()
    ## if there is a pre-calculated inverse notify the user
    ## with a message, return the stored value, and exit
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## load the initial matrix into data
    data <- x$get()
    ## solve the matrix inverse and assign to m
    m <- solve(data, ...)
    ## set the inverse within the object for future use
    x$setinv(m)
    ## return the newly calculated inverse
    m       
}
