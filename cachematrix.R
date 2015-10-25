## The purpose of these functions are to reduce the computation requirements  
## of an matrix inversion calculation by caching the inverse of the matrix

## The makecacheMatrix function creates a special matrix 
## where the invers of the matrix will be cached

makeCacheMatrix <- function(x = matrix()) {
        invrm <- NULL                   ## Sets invrm to a null as a "space holder" in memory for later cached matrix data
        set <- function(y) {            ## Function that creates a null matrix for the inverse of the imput matrix
                x <<- y                 ## Redefines x in the parent environment with y   (Ref: RStudio Help "<<-")
                invrm <<- NULL          ## Redefines invrm in the parent environment with null 
        }
        get <- function() x             ## Gets value of the matrix
        setmatrix <- function(inverse) invrm <<- inverse  ## Function sets the space holder matrix in the cache
        getmatrix <- function() invrm   ## Function gets the cached matrix 
        list(set = set, get = get,      ## Lists functions and environments
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}
## The cacheSolve function checks if the matrix inverse was calculated
## then bypasses the inverse calculation using the cached data. If not cached 
## the invsere is calculated and cached.

cacheSolve <- function(x, ...) {
        invrm <- x$getmatrix()                  ## Gets cached matrix that holds matrix inverse
        if(!is.null(invrm)) {                   ## Checks if matrix is null or contains inverse
                message("getting cached data")  ## Prints message when matrix is not null, inverse is cached
                return(invrm)                   ## Returns matrix inverse data
        }
        data <- x$get()                         ## Gets data to calculate maxtrix inverse
        invrm <- solve(data, ...)               ## Solve calculates inverse of matrix when solve is passes one parameter that is a numeric
        x$setmatrix(invrm)                      ## The matrix inverse is cached in memory
        invrm                                   ## Return a matrix that is the inverse of 'x'
}


## References: RStudio Help "<<-"
## http://www.codebytes.in/2015/08/r-programming-matrix-inversion-cache.html
## https://stat.ethz.ch/pipermail/r-help/2015-February/426078.html
## https://rpubs.com/yelangya3826850/CacheTime-consumingComputahttps://stackoverflow.com/questions/25374803/returning-the-inverse-matrix-from-a-cached-object-in-r-checking-that-input-matritions
## https://stackoverflow.com/questions/23796316/returning-the-inverse-matrix-from-a-cached-object-in-r

