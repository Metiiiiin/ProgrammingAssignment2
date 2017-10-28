##The two functions below first saves a matrix inputed and then perform an inverse on it

## First we get the matrix and save it
makeCacheMatrix <- function (x=matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL}
        get <- function()x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)}

## in here we inverse the matrix and output it
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)}
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv}
