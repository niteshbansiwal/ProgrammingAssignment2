makeCacheMatrix<- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse =  setinverse,
         getinverse  =  getinverse)
}

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
x<-makeCacheMatrix( matrix(c(-3,5,1,0),2,2))
x$set(matrix(c(-3,5,1,0),2,2))
x$get()
cacheSolve(x)



