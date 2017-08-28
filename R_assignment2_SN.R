#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y) {
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setMtx <- function(solveMtx) mtx <<- solveMtx
  getMtx <- function() mtx
  list(set = set, get = get,
       setMtx = setMtx,
       getMtx = getMtx)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mtx <- x$getMtx()
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data)
  x$setMtx(mtx)
  mtx
}


## Test:

# > source('~/.active-rstudio-document')
# > x <-  rbind(c(1, 2), c(2, 1))
# > mt <-  makeCacheMatrix(x)

# > mt$get()
#       [,1] [,2]
# [1,]    1    2
# [2,]    2    1

# first run - no chching
# > cacheSolve(mt)
#         [,1]       [,2]
# [1,]  -0.3333333  0.6666667
# [2,]   0.6666667 -0.3333333


# second run - caching
# > cacheSolve(mt)
# getting cached data
#         [,1]       [,2]
# [1,]  -0.3333333  0.6666667
# [2,]   0.6666667 -0.3333333


