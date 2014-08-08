## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## Compared to the example x was declared as "matrix" instead of "numeric" vector

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, and has not been changed, cachesolve 
## should retrieve the inverse from the cache.
## Compared to the example, "solve" was substituted for "mean"

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## To test that the function works properly a test matrix was created
mtrxdata = c(1:4)
mtrx1 = matrix(mtrxdata, nrow = 2, ncol=2)
mtrx1

## Then the inverse of this matrix was calculated
inv.mtrx1 = solve(mtrx1)
inv.mtrx1

## Then makeCacheMatrix and cacheSolve functions were employed
mtrx2 = makeCacheMatrix(mtrx1)
cacheSolve(mtrx2)

## Results match for "new" matrix so the "cached data" aspect was tested
mtrx3 = mtrx2
cacheSolve(mtrx3)
