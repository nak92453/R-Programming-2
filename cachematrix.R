

#setting null values to create empty data fields, input cache data, and defining functions 
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  #set matrix value in cache 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get matrix from cache when called
  get <- function(){
    x
  } 
  setcache <- function(matrix){
    m <<- matrix
  }
  getcache <- function() {
    m
  }
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}




## Write a short comment describing this function

#If there is no matrix inverse calculation, calculate inverse of matrix(x)
cacheSolve <- function(x, ...) {

  m <- x$getcache()
  
  #check if cache have inverse matrix and if so, print it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #there is no matrix inverse in cache, so running inverse calculation and printing inverse 
  data <- x$get()
  m <- solve(data)
  x$setcache(m)
  m
}
