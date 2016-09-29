## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  #set the matrix
  set<-function(y){ 
    x<<-y
    m<<-NULL
  }
  
  #get the matrix
  get <- function() x 
  
  #set the inverse matrix
  set_matrix <- function(inverse_matrix = matrix())  m <<- inverse_matrix  
  
  #get the inverse_matrix
  get_matrix <- function()  m   
  
  #create the list
  list(set = set, 
       get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)
  
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Check if the inverse already exists
    m <- x$get_matrix()
    
    if(!is.null(m)){ 
     
  #when it exists, return the cache matrix as the function output
      message("getting cached matrix")
      return(m)
     
    }
    
  # when it does not exist, calculate the inverse through solve function and return set the cache
    data <- x$get()
    m <- solve(data, ...)
    x$set_matrix(m)
    m
}
