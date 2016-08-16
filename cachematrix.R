## This is my assignment for Lexical Scoping - Frederica Janga

## The first function creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


## The second function returns the inverse of the matrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it  gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and 
## sets the value of the inverse in the cache via the setinverse function.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
 
	inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
