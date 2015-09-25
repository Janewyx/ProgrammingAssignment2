## The two functions together produce an inverse of a matrix that is assigned

## This function assigns new values to the function's matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){                       
    x <<- y
    inv <<- NULL
  }
  get <- function () x                 #prints what is assigned to x
  setinv <- function(assignedvalue) inv <<- assignedvalue #where the new value assignment takes place 
  getinv <- function() inv             #displays the newly assigned values
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This is where the matrix inverse takes place

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinv() 
  if (!is.null(inv)) {               #the first time inv is null, so proceeding to the "solve" function
    message("getting cached matrix") #this message is printed the second time the function is run since inv is not null anymore
    return(inv)
  }
  data <- x$get()
  inverse <- solve(data, ...)  #inverse matrix here
  print(inverse)               #prints inverse values
  x$setinv(inverse)
}

#run the following five lines for the first time 
y <- matrix(1:4,2,2)
a <- makeCacheMatrix()
a$set(y)
a$get()
cacheSolve(a)

#then run this to get the message, because inv isn't null anymore after the first run. 
a$getinv()  
cacheSolve(a)
