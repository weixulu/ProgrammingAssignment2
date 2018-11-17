## 1. makeCacheMatrix() creates a "matrix" that is in facat a list of 4 functions
## 2. cacheSolve() gets the cached invserse of a "matrix", 
##      if there is no cached inverse, one will be calculated and stored. 

## This function creates a list of four functions
makeCacheMatrix <- function(x = matrix()) {
    inv.mx <- NULL  # inverse of the matrix
    set <- function(y) { # set new content of the matrix
        x <<- y
        inv.mx <<- NULL
    }
    get <- function() x # get the content of the matrix
    setInvMatrix <- function (input.mx) inv.mx <<-input.mx  # cache the invserse
    getInvMatrix <- function() inv.mx  # get the cached inverse
    list(set = set, get = get, 
         setInvMatrix = setInvMatrix, 
         getInvMatrix = getInvMatrix)
}


## This function gets the cached inverse of a "matrix"
## If there is none, one will be calculated and cached. 
cacheSolve <- function(x, ...) {
    inv.mx <- x$getInvMatrix() # attempt to get the cached inverse
    if(!is.null(inv.mx)){ # if there is a cached inverse, return it
        message("getting cached idata")
        return(inv.mx)
    }
    x.mx <- x$get() # otherwise, get the matrix from x, then solve it
    inv.mx <- solve(x.mx)
    x$setInvMatrix(inv.mx) # cache the inverse in x. 
    inv.mx 
}
