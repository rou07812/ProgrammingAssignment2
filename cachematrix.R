# Assuming a special matrix is square these functions creates an inverse of a matrix and caches it
# On the 2nd call onwards if the matrix have been calculated the inverse is retrieved from cache

# This function creates a special matrix (list) containing functions
makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL        #Initiate blank inverse matrix
        set <- function(y) {    #set values of matrix
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x     #get values of matrix
        setinverse <- function(inverse) inverseM <<- inverse    #set inversed matrix
        getinverse <- function() inverseM       #get inversed matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #create list of functions
}

# This function calculate the inverse of the matrix supplied by makeCacheMatrix
cacheSolve <- function(x, ...) {
        inverseM <- x$getinverse()      #Get matrix from makeCacheMatrix
        if (!is.null(inverseM)) {       #Check if matrix not created
                message("#Getting inverse of matrix from cached data#") #message to console
                return(inverseM)        #Return inverse matrix from cache
        }
        data <- x$get()         #Get matrix from makeCacheMatrix
        i <- solve(data, ...)   #Inverse new matrix
        x$setinverse(i)         #set inversed matrix to cache
        i                       # Return a matrix that is the inverse of 'x'
}
