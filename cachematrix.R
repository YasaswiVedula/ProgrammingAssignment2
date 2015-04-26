## This code computes the inverse of a invertible square matrix in lesser time when the same matrix is provided.
## We create a 'special' matrix and compute its inverse and the results will be stored in the Cache Memory to retrieve the same later.

## makeCacheMatrix() stores the results and provides them quickly without computing the inverse for same matrices

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve() function computes the inverse of a given square invertible matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
