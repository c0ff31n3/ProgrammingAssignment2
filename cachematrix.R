## Put comments here that give an overall description of what your
## functions do

## makeMatrix function create a matrix and computes the matrix inverse
## matri inverse is saved as a parameter of the enclosing functions 
## which means that this parameter might be used with enclosing enviroment if exists

makeMatrix <- function(x = matrix()) {

## Create matrix and computes the inverse matrix

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve checks the parent environment first if parameter m matrix inverse exists
## in case if it was already computed than the function returns the inverse
## in case if parameter doesn't exists it computes the inverse with solve function
## the result of computation is saved with x$setsolve(m) as a parameter of makeMatrix

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
