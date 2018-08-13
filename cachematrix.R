## Author: Sibonelo Ngobese
## R-programming assessment 2

## Two functions that cache the inverse of a matrix


## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    inv <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate the inverse of the matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix is still 
## unchanged), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
## Let's create a three by three matrix for testing this in-memory matrix function

## vect <- c(3., 4., 1, 2., 3., 21, 5, 9, 10)
## data <- matrix(vect, nrow=3, ncol=3)
## inmemorydata <- makeCacheMatrix(data)

## Now print inverse from cache
> cacheSolve(inmemorydata)

              [,1]          [,2]         [,3]
[1,]  1.000000e+00  1.110223e-16 3.885781e-16
[2,] -4.163336e-17  1.000000e+00 0.000000e+00
[3,]  1.327063e-16 -1.110223e-16 1.000000e+00

## Done
