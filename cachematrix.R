## 
##In the following lines I define two functions that create a special object 
## that enables to cache the inverse of a numeric matrix.
######################     makeCacheMatrix       ###############################
## Function "makeCacheMatrix" creates the special object-"cache matrix", that
## caches inverse of its argument and returns a list of elementary operations 
## over the  matrices,such as setting/fetching value of a given matrix or its cache.

makeCacheMatrix <- function(x = matrix()) {
    # x is the matrix whose inverse is cached
    #
    #returns the special object "cache matrix", which contains cached value for 
    #x's inverse (variable 'inv') and  list made of elementary operations over x
    #and the cached value.The list contains the following 4 operations:
    #   1. set(y): Sets the matrix x to the value y
    #   2.get(): Prints/Fetches the value of x
    #   3.setInverse(z): Sets the  cached value of x's inverse matrix to z
    #   4.getInverse(): Fetches the cached value of x's inverse matrix
    
    inv <- NULL                           #'inv' caches the inverse of x
    
    #1. set(y)
    set <- function(y) {
        #sets value of x to y
        #resets value of inv to NULL
        x <<- y
        inv<<- NULL
    }
    
    #2. get()
    get <- function() x                 # returns/prints x
    
    #3. setInverse(z)
    setInverse <- function(z) inv <<- z #sets value of inv to z
    
    #4. getInverse()
    getInverse <- function() inv        #fetches inv
    
    
    list(set = set, get = get,         #makeCacheMatrix returns the special object
         setInverse = setInverse,      #in the form of a list
         getInverse = getInverse)
    
}

###########################     cacheSolve       ###############################
## cacheSolve indeed calculates the inverse of the "cache matrix" returned by 
##makeCacheMatrix above. If the inverse has already been evaluated  and stored in 
## cache (and the matrix has not changed), then the cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
    # x is a "cache matrix" which is created by makeCacheMatrix 
    # Returns a matrix that is the inverse of 'x$get()'
    inv <- x$getInverse()       #fetches cached value of the inverse
    if(!is.null(inv)) {
        #the inverse was previously calculated 
        message("Getting cached data")
        return(inv)
    }
    #the inverse was not calculated
    data <- x$get()             #fetches the matrix whose inverse is asked for
    inv <- solve(data)          #calculates the inverse 
    x$setInverse(inv)           #caches the inverse
    inv                         #retuns the inverse
     
}
