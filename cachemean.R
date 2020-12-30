makeVector <- function(x = numeric()) {
  # define fields
  cache <- NULL
  orig <- NULL
  # define functions
  set <- function(y) {
    orig <<- y
    cache <<- NULL
  }
  get <- function() {
    orig
  }
  cacheValue <- function(value){
    cache <<- value
  }
  getCachedValue <- function(){
    cache
  }
  # make our object
   r<-  list(set = set, get = get,
       cacheValue = cacheValue,
       getCachedValue = getCachedValue)
  print(r)
}

cachemean <- function(x, ...) {
  m <- x$getCachedValue()

  if(!is.null(m)) {
    # We have already calculated this before and stored it in the cache. Just get it from the cache.
    message("getting cached data")
    return(m)
  } else {
    # we have NOT already calculated this before, so calculate it
    data <- x$get()
    m <- mean(data, ...)
    # now store the value in our cache
    x$cacheValue(m)
    return(m)
  }
}