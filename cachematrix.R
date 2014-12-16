## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL 											## Initializing variable for inverse matrix with NULL
        set <- function(y) {								## Set a different matrix 
                x <<- y 									## Initialize with new Matrix
                i <<- NULL 									## Initializing the inverse matrix as NULL
        }
        get <- function() x 								## Get the matrix
        setinverse <- function(inverse) i <<- inverse 		## Set the inverse of the matrix 
        getinverse <- function() i 							## Get the inverse of the matrix
        list(set = set, get = get, 							
             setinverse = setinverse,
             getinverse = getinverse) 						

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { 							## Function to return the inverse of the matrix
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() 								## Get the inverse from the function makeCacheMatrix
        if(!is.null(i)) { 									## Check if the inverse from the function makeCacheMatrix is NULL
                message("getting cached data") 				
                return(i)
        }
        data <- x$get() 									## Get the matrix if inverse is not present
        i <- solve(data, ...) 								## Calculate the inverse of the matrix
        x$setinverse(i) 									## Set the inverse of the matrix in the function makeCacheMatrix
        i 													## Return the inverse of the matrix
}
