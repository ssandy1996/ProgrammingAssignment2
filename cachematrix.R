## Functions to cache the inverse of a matrix by taking advantage
## of the scoping rules of R 

## Function that creates a "vector" which is basically a list 
## containing the functions to set/get the value of the matrix
## and to set/get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {    ## Function to set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x    ## Function to get the value of the matrix
    setInv <- function(inv) m <<- inv    ## Function to store the inverse 
                                         ## of the matrix            
    getInv <- function() m    ## Function to retrieve inverse of the matrix        
    list(set = set, get = get,    
         setInv = setInv,
         getInv = getInv)    ## Returing a list of the 4 functions
}


## Function to compute the inverse of the matrix created by the
## previous function but only if the matrix has a new value, otherwise
## it retrieves the inverse of the matrix stored previously
## Note: Matrix 'x' is assumed to be square invertible

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {    ## If the inverse is already computed retrieve the 
                         ## stored value and return instead of recomputing 
        message("getting cached data")
        return(m)
    }
    data <- x$get()    ## When it is not stored get the matrix value
    m <- solve(data, ...)    ## Compute the inverse of the matrix
    x$setInv(m)    ## Store/Cache the inverse matrix for future use
    m    ## Return the matrix that is inverse of 'x'
}
