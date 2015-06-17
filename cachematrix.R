#  Functions:
#    get - gets matrix from user
#    set - sets the matrix
#    get_sv - gets solved matrix from user
#    set_sv - sets solved matrix

## makeCacheMatrix: In this function either user can provide matrix or set matrix or inverse matrix.
##                  It stores the inversed(solved) matrix and lists all the functions used at last.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     set_sv <- function(solve) m <<- solve
     get_sv <- function() m
     list(set = set, get = get, set_sv = set_sv, get_sv = get_sv)
}


## cacheSolve: This function solves the matrix, if not already solved and displays result in end.

cacheSolve <- function(x, ...) {
     m <- x$get_sv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$set_sv(m)
     m
}
