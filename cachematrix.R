makeCacheMatrix <- function(x = matrix()) #Creates the function that creates the matrix
{
  #Stablish initial variables
  inverse <- NULL
  set <- function(y) #Creates the 'set' function
  {
    x <<-y
    inverse <<- NULL
  }
  get <- function(){x} #Creates the 'get' function which returns the matrix. 
  setInverse <- function(inverse){inverse <<- inverse} #Creates the 'setInverse' function which allows to set the inverse of the matrix. 
  getInverse <- function() {inverse} #Creates the 'getInverse' function which returns the inverse of the matrix. 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) #Creates the function that calculates the inverse of the matrix given by the function abover
{
  inverse <- x$getInverse() #Returns the inverse of the matrix. 
  if(!is.null(inverse)) #Condition that evaluates if the inverse is not null. 
  {
    message("getting cached data") #Returns the message
    return(inverse) #Returns the inverse
  }
  matrix <- x$get() #Gets the matrix
  inverse <- solve(matrix, ...) #Solves the inverse
  x$setInverse(inv) #Sets the inverse
  inverse #Returns the inverse
}
