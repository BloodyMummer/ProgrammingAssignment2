## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        #initalize value that will hold inverse
        inverse <- NULL
        #this function will assign value of matrix in parent environment to set
        #if there is a new it sets inverse to null
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        #returns matrix arg value
        get <- function() x
        #assigns value of inverse form parent arguement to setinv
        setinv <- function(val) inverse <<- val
        #gets value of inverse
        getinv <- function() inverse
        #creates a list that will allow us to use $ operator to call items from 
        #cached matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#solve the cached matrix
cacheSolve <- function(x, ...) {
        #get data from x$getinv
        inverse <- x$getinv()
        #if it is not null then return message and cached matrix
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        #if it is null get data, assign to data and use solve to find inverse
        data <- x$get()
        inverse <- solve(data, ...)
        #set value to cached matrix
        x$setinv(inverse)
        #return value of inversed matrix
        inverse
}
