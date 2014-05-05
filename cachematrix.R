## The makeCacheMatrix takes a matrix as input and 
## returns a list containg 4 functions. The cacheSolve function reurns 
## the inverse of that square matrix, whether the input is computed or 
## retrived from cache.

## makeCacheMatirx takes an invertablly square matrix as input and 
## returna a list containing 4 elements, namely set,get, setinverse,
## getinverse. Each of these 4 elements is a function. set function creates the
## square matrix. getinverses prints out the square matrix. setinverse
## functions takes the inverse of the square matrix as the input and 
## assigns the inverse to the variable i. getinverse functions prints out
## the variable i, which is the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}


## The cacheSovle functoin returns the inverse of the square matrix. 
## It first calls the getinverse function, then uses a if structure to
## judge whether the inverse is computed and cached. If the variable 
## in function getinverse is not null, which means the inverse is 
## cached, a message prompted and the inverse is shown. 
## If the inverse is not computed before, the square matirx created 
## before is assigned to the data variable. The inverse(i variable) is 
## computed then passed to the setinverse function as the input and cached. 
## Finally the inverse is printed out.




cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
