## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())  
{
inv <- NULL                                            ## defining inv to store the inverse matrix
set <- function(y) 
{                                 
        x <<- y
        inv <<- NULL
}
        get <- function() x                            ## get value of matrix
        setinv <- function(inverse) inv <<- inverse    ## set value of inverse matrix      
        getinv <- function() inv                       ## get value of inverse matrix
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)                         ## Return a matrix that is the inverse of 'x'
{       
inv <- x$getinv()             
        if(!is.null(inv))                              ## checks whether inverse matrix is not null
{             
        message("getting cached data")
        return(inv)                                    ## returns inverse matrix
}
        data <- x$get()                                ## get original matrix
        inv <- solve(data, ...)                        ## using solve to get inverse of matrix
        x$setinv(inv)                                  ## set inverse matrix
        inv                                            ## returns the inverse matrix
}
