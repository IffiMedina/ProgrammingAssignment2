## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL   
      ## Sets the value of our cached matrix by changing the value of x to y
      set <- function(y) {
              x <<- y
              i <<- NULL
      }
      ## Gets matrix
      get <- function() x
      ## Sets inverse of matrix
      set_inverse <- function() i <<- solve
      ## Calls the matrix inverse
      get_inverse <- function() i
      ## Function will output a list with the outputs of the functions above
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## Computes the inverse of makeCacheMatrix(x) 
## If the inverse already exists, then this function
## should get the inverse from the cache created
## in makeCacheMatrix(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## If the inverse already exists, it sets i to the value from the cache
  i <- x$get_inverse()
  ## If i is not NULL type, returns message and i
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Creates variable data that is set to equal the cached matrix
  data <- x$get()
  ## Variable i is assigned the value of the inverse matrix
  i <-solve(data,...)
  ## Sets inverse of arguement to the value of i 
  x$set_inverse(i)
  i
}
