## cacheSolve calculates the inverse of a matrix. 
## It builds upon the standard R function solve() by adding a caching layer for the inverse.
## The caching layer is implemented in a wrapper function makeCacheMatrix that encapsulates the matrix to be inverted.
## 
## Usage sample
##
## > m <- matrix(c(1,0,5,2,1,6,3,4, 0), nrow=3, ncol=3)
## > mat = makeCacheMatrix(m)
## > cacheSolve(mat)
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## > cacheSolve(mat)
## getting cached data
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
##> mat$set(matrix(c(4,3,3,2), nrow=2, ncol=2))
##> cacheSolve(mat)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
##> cacheSolve(mat)
## getting cached data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
##

## makeCacheMatrix is a "factory" function that takes a matrix x and returns a wrapper for x that exposes 
## getter and setter functions for x, as well for the inverse of x. Iks works in conjuction with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## clear cache upon initialization 
    set <- function(y) {
        x <<- y   
        inv <<- NULL ## clear cache upon calling setter
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    ##
    ##  return getters and setters for the matrix and its inverse 
    ##
    list(set =set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}    


## CacheSolve takes a makeCacheMatrix object and returns the inverse of the matrix it wraps. 
## The first call to cacheSolve after setting the value of the makeCacheMatrix object 
## (either by calling makeCacheMatrix or through a call to the setter) will result in calling solve()
## to generate the inverse. Subsequent calls will return the cached value stored in the "inv" variable in makeCacheMatrix. 

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)  ## return cached inverse
    } 
    data <- x$get()
    inv <- solve(data)  ##calculate inverse when not present in cache
    x$setinverse(inv)
    inv
}


