# Finding the inverse of matrices can be very useful to solve many interesting data problems.
# However, repeatedly calling the solve() function can be inefficient and therefore, 
# there is a need to cache the value of a matrix if it has been calculated previously. 

# makeCacheMatrix is basically a constructor that creates objects having properties of a matrix 
# with value = x, and its inverse = im. This constructor class has four main function that 
# are meant to get and set values for x and im. The "getter" function are get(to retrieve value 
# of x) and getInvrs(to retrieve the value im). The "setter" functions are set(to initiate 
# the value of x and im) and setInvrs(to refresh the value of im after the solve function has been 
# called in the cacheSolve function). 

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function(){
                x
        }
        setInvrs <- function(invrsMtx) {
                im <<- invrsMtx
        }
        getInvrs <- function() {
                im
        }
        list(set = set, get = get, setInvrs = setInvrs, getInvrs = getInvrs)
}


# cacheSolve is the second function that works in conjunction with the makeCacheMatrix
# function to check if the passed in object(here it is x) has an existing inverse value.
# If it does have an existing value, it then returns the value "im" after displaying a message 
# saying "getting cached inverse of matrix". The function then ends. However, if im = NULL, 
# then the get constructor function retrieves the value of x in memory using the makeCacheMatrix. 
# The function then, proceeds to find the inverse value of x using the solve() function. This 
# value is then stored in im. im is then stored in the object for use at another time when 
# appropriate. Finally the inverse value is returned thus ending the function.

cacheSolve <- function(x, ...) {
        im <- x$getInvrs()
        if(!is.null(im)) {
                message("getting cached inverse of matrix")
                return(im)
        }
        matx <- x$get()
        im <- solve(matx)
        x$setInvrs(im)
        im
}
