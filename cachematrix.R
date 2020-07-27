## Assignment2: Caching the Inverse of a Matrix
## A pair of function that catch the mean if it has already been computed if not the function will compute and return it.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##initialising inverse matrix
        m <- NULL
        
        ##set the value of the matrix
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        
        ##get the value of the matrix
        get <- function() x
        
        ##set the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        
        ##get the inverse of the matrix
        getinverse <- function() m
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        ##return inverse if already set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        ##computing inverse
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}

##Examples
  
x <- matrix(sample(1:100,9),3,3)
makeCacheMatrix(x)
cacheSolve(x)

## non-atomic example
f <- makeCacheMatrix(matrix(1:4, 2, 2))
print(cacheSolve(f))

## atomic examples
print(cacheSolve(matrix(1:4, 2, 2)))
