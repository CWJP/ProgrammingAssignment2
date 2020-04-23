## Put comments here that give an overall description of what your
## functions do

##Here are two funstions: makeCacheMatrix (which creates a special "matrix" object that can cache 
##its inverse.) and cacheSolve (which computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##changed), then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
## The makeCacheMatrix function creates a vector, which returns a list containing containing 4 functions to
##   1. set the matrix
##   2. get the matrix
##   3. set the the inverse of the matrix
##   4. get the the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
      m = NULL
      set <- function(y){
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function() m <<- inverse
      getinverse <- function() m
      list(set=set, get=get, setinv=setinv, getinv=getinv)   
}

## Write a short comment describing this function
## the cacheSolve function calculates the inverse of the matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache 
## via the setinverse function.


cacheSolve <- function(x, ...) {
        m = x$getinverse()

        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }

        data = x$get()
        m = solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
