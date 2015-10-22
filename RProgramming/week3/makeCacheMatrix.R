makeCacheMatrix <- function (X) {
    inv <- NULL
    
    set <- function(Y) {
        X <<- Y
        inv <<- NULL
    }
    get <- function () X
    
    setinverse <- function (inver) inv <<- inver
    getinverse <- function () inv
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(X, ...) {
    inv <- X$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setmean(inv)
    inv
}