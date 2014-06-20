## This pair of functions will:
##  1) Create a matrix object and cache its inverse
##  2) Will compute the inverse of the special matrix returned by CacheMatrix above.  

## The makeCacheMatrix function will create the matrix and cache its inverse in similar fasion to the makeVector function. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x << - y
        m << - NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse but first checks to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
         if(!is.null(m)){
            message("getting cached data")
            return(m)
         }
         data <- x$get()
         m <- inverse(data,..)
         x$setinverse(m)
         m      
}
