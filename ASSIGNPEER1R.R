
makeCacheMatrix<- function(x=matrix()){
  Inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get<- function() {x}
  setInverse<- function(inverse)   {inv <<- inverse}
  list(set = set , get=get, setInverse = setInverse ,getInverse = getInverse)
}

cachesolve <- function(x,...){
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
    
  }
  mat <- x$get()
  inv <-  solve(mat,...)
  x$setInverse(inv)
  inv
  
}




