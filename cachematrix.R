## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#
# gets a matrix, stores it and creates 4 'functions':
# recover matrix - get
# store matrix - set
# store inverse matrix - set_inv
# recover inverse matrix - get_inv
#
makeCacheMatrix <- function(x = matrix()) {
                    i <- NULL
                    ## store matrix
                    set <- function(y) {
                            x <<- y
                            i <<- NULL
                            }
                    ## return matrix
                    get <- function() x
                    ## store inverse matrix
                    set_inv <- function(inverse) i <<- inverse
                    ## get inverse matrix
                    get_inv <- function() i
                    ##
                    list(set = set, get = get,set_inv = set_inv,get_inv = get_inv)    
}


## receive a cached matrix and returns its inverse
## calculate if it does not exist
## return stored value if already exists 

cacheSolve <- function(x, ...) {
    
                ## Return a matrix that is the inverse of 'x'
                m <- x$get_inv()
                if(is.null(m)) {
                                data <- x$get()                                       
                                x$set_inv(solve(x$get(), ...))
                                message("calculating data")
                                return (x$get_inv())                                       
                                }
                                
                message("getting cached data")
                return(m)                
}



exemplo_de_matriz <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
matriz_cache <- makeCacheMatrix(exemplo_de_matriz)
cacheSolve(matriz_cache)

