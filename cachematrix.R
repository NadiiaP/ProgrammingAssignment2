##  makeCacheMatrix create functions, which store matrix (set, get)
## and cache it's inverse (setcache, getcache)

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  
  setcache <- function(solve) c <<- solve
  getcache <- function() c
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## ## cacheSolve check the cache and return it's value, if cache is NULL 
## calculate inverse matrix and put it to the cache

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  c <- makeCacheMatrix(x)$getcache()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- makeCacheMatrix(x)$get()
  c <- solve(data, ...)
  makeCacheMatrix(x)$setcache(c)
  c
}
