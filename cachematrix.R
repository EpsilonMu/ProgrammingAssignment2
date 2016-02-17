## Programming Assignment 2: Lexical Scoping

## The makeCacheMatrix function creates a list of 4 functions that can then be called  
## by the cacheSolve function.
## 1. set - re-sets the value of x
## 2. get - finds the current value of x
## 3. setsolve - sets the value of inv to the inverse matrix
## 4. getsolve - returns the value of inv, if null the matrix has not yet been cached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function is used to evaluate whether the variable inv has been 
## cached, if it has not it then calculates and caches the inverse matrix.
## 1. returns the value of inv, if null the matrix has not yet been cached
## 2. If inv is not null returns the value of inv and exits the function
## 3. If inv is null, calculates inv and caches the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}


