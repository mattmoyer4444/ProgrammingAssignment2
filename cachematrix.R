## Put comments here that give an overall description of what your
## functions do
# Example usage:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cache_x <- makeCacheMatrix(x)                  // Create cached matrix
# > cache_x$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
# 

## Write a short comment describing this function

# From assignment notes: In this example the <<- operator is introduced which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric matrix and cache's its inverse

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse
        set_inverse<- function(inverse) inv_matrix <<- inverse
        # get the value of the inverse
        get_inverse <- function() inv_matrix
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

# The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        if (!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        } 
                data <- x$get()
                inv_matrix <- solve(data, ...)
                x$set_inverse(inv_matrix)
                inv_matrix
}

# Test cases
x <- matrix(rnorm(16), nrow = 4)          # Create a matrix x
cache_x <- makeCacheMatrix(x)                  # Create cached matrix
cache_x$get()                                  # Return the matrix
cacheSolve(cache_x)                            # Return the inverse
cacheSolve(cache_x)


