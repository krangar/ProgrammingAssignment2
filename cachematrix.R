# makeCacheMatrix outputs a list containing 4 functions

# set - sets the value of the matrix

# get - gets the value of the matrix

# set_matrix_inv - sets the matrix inverse

# get_matrix_inv - gets the matrix inverse

makeCacheMatrix <- function(x  =  matrix()) {

    inv <- NULL

    set <- function(y) {

        x <<- y

        inv <<- NULL

    }

    get <- function() x

    set_matrix_inv <- function(inverse) inv <<- inverse

    get_matrix_inv <- function() inv

    list(set = set, get = get, set_matrix_inv = set_matrix_inv, get_matrix_inv = get_matrix_inv)

}


# cachesolve is a function that returns the inverse of a matrix if the  inverse
# has already been computed. If not, it computes the inverse, sets the matrix inverse in a cache and returns the inverse


cacheSolve <- function(x, ...) {

    inv <- x$get_matrix_inv()

    if(!is.null(inv)) {

        message("getting cached data.")

        return(inv)

    }

    data <- x$get()

    inv <- solve(data)

    x$set_matrix_inv(inv)

    inv

}
