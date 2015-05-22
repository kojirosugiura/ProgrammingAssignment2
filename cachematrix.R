## The below 2 functions calculate the inverse of a matrix and save it to cache
## so as to reuse the calculated value without repeating the same calculation.

## makeCacheMatrix function creates a special "matrix" that lists a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m  <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## cacheSolve function calculates the inverse of the special "matrix"
## created by makeCacheMatrix while avoiding repeating the same calculation
## in the case that the inverse has already been calculated and stored in cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("fetching cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
