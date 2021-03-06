# Caching the complex computations could benefit whenever they need to be used repeatedly, 
# rather than computing them again and again. Here we are creating two functions: 
# "makeCacheMatrix" and "cacheSolve" which will enable us to compute inverse of an invertible matrix 
# and then cache that inverse in case it needs to be used again. 
# Assumption is that given matrix is always invertible. 

# First Function: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x= matrix()) {

# Initialize objects  
        
        InvertedMatrix <- NULL
# Set the matrix    
        setMatrix <- function(y) {
                
                x <<- y
                InvertedMatrix <<- NULL
                }
# Retrieve the matrix
        
        getMatrix <- function() x

# Set the inverted matrix
        
        setInvertedMatrix <- function(Invert) InvertedMatrix <<- Invert 

# Retrieve the inverted matrix   

        getInvertedMatrix <- function() InvertedMatrix
        
# create a new object by returning the list
        
        list(setMatrix= setMatrix,
             getMatrix= getMatrix,
             setInvertedMatrix= setInvertedMatrix,
             getInvertedMatrix= getInvertedMatrix)                
        }

# Second function:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {

# Return a matrix that is inverse of x 
        
        InvertedMatrix <- x$getInvertedMatrix()

#Check whether cache is available and return it if available
        
        if(!is.null(InvertedMatrix)) {
                message("fetching cached data")
                return (InvertedMatrix)
        }
# Compute inverse if cache is not available        
        else {

                M <- x$getMatrix()
                InvertedMatrix<- solve(M,...)
                x$setInvertedMatrix(InvertedMatrix)
                InvertedMatrix
                        }
}

