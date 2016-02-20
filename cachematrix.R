## makeCachematrix is a function that creates a matrix object that will be used
## in conjunction with cacheSolve to calculate the inverse of a matrix. 
## If there is already an inverse of the matric, then it will run the cache 
## and return. It will not recalculate. 

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_x <<- inverse
    getinverse <- function() inv_x
    list(set=set, get=get, 
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve gives the inverse of a matrix A that was created with the 
## makeCacheMatrix function. If there is a cached inverse available, then 
## cacheSolve retrives it. If there is not then cacheSolve computes, caches, 
## and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)){
        message("Getting cached inverse matrix.")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}

##taraxmeyer
##tara.ashley.meyer@gmail.com
