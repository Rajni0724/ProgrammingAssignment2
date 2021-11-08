#in the first chunk of code I will write a function to makeCacheMatrix which creates a matrix object that can cache its inverse
#the code has to acheive the following 
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #originally setting the value of the inverse as null
  set <- function(y) { #using set to set the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() (x) #getting the value of a matrix
  setinverse <- function(inverse) (i <<- inverse) #set the value of the inverse
  getinverse <- function() (i) #get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#in this second chunk I will write the code to computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheinverse <- function(x, ...) {
  i <- x$getinverse() #looking to see if the inverse of the matrix has already been calculated
  if(!is.null(i)) { #if it has
    message("getting cached data") #type the message that it is getting the cached data
    return(i) #return the cached value of the inverse
  }
  mat <- x$get() #in the case that it has not yet been cached 
  i <- solve(mat, ...) #solve for the inverse of the matrix
  x$setinverse(i)
  i
}
  #Output
  #> m1<- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
  #> cacheinverse(m1)
  #[,1] [,2]
  #[1,]   -2  1.5
  [#2,]    1 -0.5
  
  
  #cacheinverse(m1)
  #getting cached data
  #[,1] [,2]
  #[1,]   -2  1.5
  #[2,]    1 -0.5
  
