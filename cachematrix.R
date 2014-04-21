## Reply to Peer Assessed 2nd Programming Assignment for R Programming
## nicholas.paul.hartman

## Create list of functions to set, get, invert, and retrieve the 
## inversion of a Matrix

makeCacheMatrix <- function(x = matrix()) {     # function name & arguments
        
        m <- NULL                               # create cache object
        set <- function(y) {                    # create 'set', list.func 1
                x <<- y                         # assign new values to x
                m <<- NULL                      # reset cache object
        }
        get <- function() x                     # create 'get', list.func 2
        setinverse <- function(solve) m <<- solve
                # create 'setinverse', list.func 3; eval, assign cache obj
        getinverse <- function() m
                # create 'getinverse', list.func 4; retrieve cache obj
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   # compile func list
        
}


## Return cached - or cache if uncached and then return - 
## inversion of a matrix

cacheSolve <- function(x, ...) {                # function name & arguments
        
        m <- x$getinverse()             # create, assign func returned obj
        if(!is.null(m)) {               # if rtrnd obj pre-calc'ed,
                message("getting cached inverse")       # indicate and
                return(m)                               # return obj
        }                                               # else
        data <- x$get()         # retrieve values (matrix) for evaluation
        m <- solve(data, ...)   # invert values (matrix), assign rtrnd obj
        x$setinverse(m)         # assign cache obj for future retrieval
        m                       # return matrix inversion
        
}