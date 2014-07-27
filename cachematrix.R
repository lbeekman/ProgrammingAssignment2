## The following functions use objects in cached memory
##    makeCacheMatrix: makes a special object "matix" and caches its solution
##    cacheSolve:  solves the inverse a matrix(using cached solution if available)


## Create special object that stores a matrix and caches its solution

makeCacheMatrix <- function(x = matrix()) {
          ## Cache a special "matrix" 'x'
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
}


## Calculate the inverse of the special "matrix" that was created above


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
}
