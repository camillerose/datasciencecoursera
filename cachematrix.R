## MakeCacheMatrix is a function creates a special "matrix" object 
## that can cache its inverse.

## set(): sets a new value to  and erases any value assigned to i by assigned NULL
## get(): accesses the value of x 
## set_inv(): sets the value of i to the new value of the new matrix
## get_inv(): accesses the value of i

## A list is returned that contains the above 4 functions which can then
## be accessed in cachematrix

makeCacheMatrix <- function(x = matrix()) { #default is matrix 
  
  m <<- NULL #instantiate the matrix variable and set it to NULL; clears
  #old matrix value
  
  set<-function(y){
    x <<- y #set a new matrix value to X
    m <<- NULL # erase any value assigned to m; clears cache
  }
  
  get <- function() x #function to get the value of x
  set_inv <- function(matrixinverse) m <<- matrixinverse #set the value of the inverse
  #set_inv is only used when there is no cached inverse
  get_inv <- function() m #function to return the value of the inverse
  list(set = set, get = get, #list of 4 functions is returned
       set_inv = set_inv,
       get_inv = get_inv)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { #x is the result of makeCacheMatrix
        
  m <- x$get_inv() #get cached inverse value
  
  if(!is.null(m)) { #if inverse value is not NULL/empty then return cached value
    message("getting cached data")
    return(m)
  }
  #if there is no cached inverse
  data <- x$get()  #then get the matrix value
  m <- solve(data, ...) #compute the matrix inverse
  x$set_inv(m) #set/cache the value of the inverse
  m #return inverse value
}
