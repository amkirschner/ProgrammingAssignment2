## Put comments here that give an overall description of what your
## functions do

## Two functions: makeCacheMatrix takes a matrix as an input, generates functions to
## related to getting/setting matrix and its inverse. cacheSolve checks to see if a
## value is already stored in the inverse variable, i, and either sets the inverse or 
## not depending on whether it has already been set.


## Write a short comment describing this function
## 
## This function takes an existing matrix as an input, sets its value to said input
## and creates four functions at the same time. Also initialize variable to hold inverse,
## Only the first function, set(), is run at the initial call of the function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function runs the getinverse function of the prior function to determine
## if the variable for the inverse, i, has been set; if so, it returns said value 
## and states it is getting cached data.
## If not, it calls the setinverse function which then stores the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
