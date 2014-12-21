# Together, makeCacheMatrix() and cacheSolve() creates a matrix, finds the inverse 
# of that matrix, and stores the inverse in cache. Whenever a matrix inverse is
# needed, the cache is checked first to determine whether the inverse has already
# been computed. If so, the cached inverse is returned. If not, the inverse is
# calculated and stored in cache. 

# Thanks go to Pavel Kirjanas and Bill Hilton for their great posts on the
# discussion forum: https://class.coursera.org/rprog-016/forum/thread?thread_id=96

makeCacheMatrix <- function(x = matrix()) {      # Input x is a matrix.
        
        inv <- NULL     #  'inv' will be the inverse of x. It is reset to NULL 
                        #  each time makeCacheMatrix is called
        
        #  The following 3 functions are defined by makeCacheMatrix, but
        #  are not run. Instead, they will be used by cacheSolve() to get the
        #  cached inverse (if one exists) or set the cached inverse. 
        #  These are usually called object 'methods'
        
        get <- function() { x }   # Returns the value of the original matrix.
        
        setInv <- function(solve)  { inv <<- solve }
        #  Stores the inverse of x in cache as 'inv'.
        
        getInv <- function() { inv } # Returns the cached inverse.
        
        list(get = get,          #  This is a list of the methods created above.       
             setInv = setInv,   
             getInv = getInv)                              
}


cacheSolve <- function(x, ...) {   # Input x is an object created by makeCacheMatrix().
        inv <- x$getInv()          # Gets the cached inverse for x.
        print(inv)
        if(!is.null(inv)) {        # If cached inverse was previously set
                                   # (i.e., 'inv' is not NULL) ...
                
                message("getting cached data")  # ... send this message to the console ...
                return(inv)                     # ... and return the cached inverse.
                                                # Note that "return" ends cacheSolve()
        }
        
        #If x$getInv() returns NULL:
        data <- x$get()         # Gets the original matrix passed to makeCacheMatrix().
        inv <- solve(data, ...) # Finds the inverse of the original matrix.
        x$setInv(inv)            # Stores the inverse in cache
        inv                     # Returns the inverse
}

# #To test code:
# m1 <- makeCacheMatrix(rbind(c(1, 0.5), c(0.5,1)))
# cacheSolve(m1) #run again to ensure cached inverse is called
# m2 <- makeCacheMatrix(rbind(c(1,0.5,0), c(0.5, 1, 0), c(0, 0.5, 1)))
# cacheSolve(m2) #run again to ensure cached inverse is called
# m2 <- makeCacheMatrix(rbind(c(1,0.4,0), c(0.4, 1, 0), c(0, 0.4, 1))) #changes m2 matrix
# cacheSolve(m2)

