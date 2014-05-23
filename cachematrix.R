## makeCacheMatrix function creates a "special" matrix that is not singular 
## and reversible

## How to use these functions:
## res <- makeCacheMatrix(matrix(1:25,nrow=5,ncol=5))
## res1 <- makeCacheMatrix(matrix(rnorm(25),nrow=5,ncol=5))
## cacheSolve(res)
## cacheSolve(res1)
## res$get()
## res1$getInverse()

## get(), set(), getInverse() and setInverse() are define in makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        # m is the temporary holder of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        #setInverse and getInverse sets and gets the inverse of the matrix resp.
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve takes a matrix provided by makeCacheMatrix asinput 
## and provices the inverse if the input is not a singular matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## If the inverse is already available in the cache return it.
        if(!is.null(m)) {
                message("Getting Cached Data")
                return(m)
        }
        ## Else Proceed to compute the inverse
        data <- x$get()
        
        ## Make sure the matrix input is not singular because for such matrices 
        ## inverse does not exist.
        ## If its a singular matrix you will be prompted to input a non singular matrix.
        
        ## I use matrixcalc package to check the singularity of the matrix
        ## Making sure "matrixcalc" is loaded in your environment.
        ## If not available, an internet connection is required.
        
        if(!require("matrixcalc")){
                print("Trying to install matrixcalc")
                install.packages("matrixcalc")
                if(require(matrixcalc)){
                        print("matrixcalc installed and loaded")
                } else {
                        stop("Could not install matrixcalc")
                }
        }
        if(is.singular.matrix(data)){
                message("Input Matrix is Singular. Please enter a non-singular Matrix!!")
        }
        
        ## Input Matrix is not singular and hence the inverse is calculated.
        ## solve() is used to compute the matrix
        else{
                m <- solve(data, ...)
                x$setInverse(m)
                m
        }
}