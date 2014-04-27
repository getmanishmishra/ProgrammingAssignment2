# Here we are writing two functions which will be used to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i<-null
        # Defining set function
        set<- function(y){
            x<<-y
            i<<-null
        }
        
        # defining get function
        get <- function() x
        # Define setInverse Function which will set inverse of matrix
        setInverse <- function(inverse) i <<- inverse
        # Define getInverse Function which will get inverse of matrix
        getInverse <- function() i
        #Listing all the avialable functions 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}



# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
       
        i <- x$getInverse() # Get the inverse of X by calling getInverse() function of X
        # If inverse of matrix found from above line of code, return that directly and exit the functions
        if(!is.null(i)) {
                message("getting cached data")
                # Return cached inverse matrix
                return(i)
        }
        # If inverse found from above code is null, then calculate using solve function
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        
        i # Return a matrix that is the inverse of 'x'
}
