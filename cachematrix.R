
##makeCacheMatrix - this function creates a list with elements that allow matrix creation , calculation of revesre, and teching of original and reversed matrix
makeCacheMatrix <- function(x = matrix()) {
##first, we create an empty matrix x. Then, we add a null value r  and functions set, get, setreverse and getreverse. you can call them as functions, e.g. a$set() - the behaviour is similar to class methods. 
        r <- NULL
##use a$set(matrix) to input a matrix which will be later reversed
        set <- function(y) {
        
##values are stored in higher environment (makeMatrix function namespace) to allow calling in other subfunctions 
                x <<- y
                r <<- NULL
        }
        get <- function() {x}
##use a$setReverse() to calculate the inverse of input matrix x. You can retrieve it later by a$getReverse()
        setReverse <- function() {
        bDim=nrow(x)
        b=diag(bDim)
        r <<- solve(x,b)
        }
        getReverse <- function() {r}
        list(set = set, get = get,
             setReverse = setReverse,
             getReverse = getReverse)
}

##cacheSolve: this function checks if a cache exists, retrieves existing one or creates new if one does not exist
cacheSolve <- function(a, ...) {
##we check if a reverse matrix exists and if it has a cache. a$set() property of clearing cache ensures, that if a  new matrix has been loaded,no cache would exist
        r <- a$getReverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
##if no cache exists, we compute the new reverse matrix and cache it
		a$setReverse()
        r <- a$getReverse()
        r
}
