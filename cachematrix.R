## USe of cache to store inverse matrix to avoid repetition of calculation
##
##
##  how to use:
## 1 - create a square matrix ex.: exemplo_de_matriz <- matrix(c(5, 2, 2, 2, 7, 3, 2, 3, 7), 3, 3)
## 2 - create the cache matrix ex.: matriz_cache <- makeCacheMatrix(exemplo_de_matriz)
## 3 - invert the matrix Ex.:cacheSolve(matriz_cache)
##    a - running for the first time should indicate 'inverting matrix'
##    b - running more time should retrn 'getting cached inverted matrix'
##
##  CODE DOES NOT VERIFY IF MATRIX IS SQUARE AND INVERTABLE

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
                                message("inverting matrix")
                                return (x$get_inv())                                       
                                }
                                
                message("getting cached inverted matrix")
                return(m)                
}



exemplo_de_matriz <- matrix(c(5, 2, 2, 2, 7, 3, 2, 3, 7), 3, 3)
matriz_cache <- makeCacheMatrix(exemplo_de_matriz)
cacheSolve(matriz_cache)

