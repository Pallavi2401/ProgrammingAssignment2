## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {                  ## defining argument with "matrix"
inv <- NULL                                                  ## initializing inv as NULL to hold value of inverse matrix
        set <- function(y) {                                 ## defining the set function to assign new value of matrix in parent environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x                                  ## returns value of the matrix argumen
        setinv <- function(inverse) inv <<- inverse          ## assigns value of inv in parent environment
        getinv <- function() inv                             ## gets the value of inv where called
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()                 
        if(!is.null(inv)) {                                 ## checks whether inverse matrix is not null
                message("getting cached data")
                return(inv)                                 ## returns inverse matrix
        }
        data <- x$get()                                     ## get original matrix
        inv <- solve(data, ...)                             ## using solve to get inverse of matrix
        x$setinv(inv)                                       ## set inverse matrix
        inv                                                 ## returns the inverse matrix
}
}
