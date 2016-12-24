##Assignment2: Lexical Scoping
##I'm creating a R-code that will do the following functions:

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
##    makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
##    changed), then cacheSolve should retrieve the inverse from the cache.

## BELOW IS MY CODE FOR THE ABOVE ASSIGNMENT- NEHRU PRABAKARAN

makeCacheMatrix <- function(x = matrix()) {
  IM <- NULL                                
  #IM creates a place (which is currently NULL) to store the matrix value created 
  #at the later part of this program.
  set <- function(y) {
    x <<- y           
    IM <<- NULL
  }
  
  # upon execution the set()function does the folling two things:
  # 1. Assign the input argument to the x object in the parent environment, and
  # 2. Assign the value of NULL to the IM object in the parent environment.
  
  get <- function() x
  
  #the above get function takes advantage of the lexical scoping;note that the symbol x is 
  #not defined within get(), it is retried from the parent environment of makeCacheMatrix()
  
  setinverse <- function(inverse) IM <<- inverse
  getinverse <- function() IM
  
  #IM value is defined in the setter function - setinvers(), and retrived in getter- getinverse()
  
  list(set = set,              #gives the name 'set' to the set() function defined above            
       get = get,              #gives the name 'get' to the get() function defined above
       setinverse = setinverse,# gives the name 'setinverse' to the setinverse() function 
       getinverse = getinverse)# gives the name 'getinverse' to the getinverse() function
}

 #All the defined functions are listed as an element within a list(), and returns it to the 
 #parent environment makeCacheMatrix().
 #Naming the list elements allows us to use the operator $ to access the functions by 
 #name in the second part of the code, i.e. cacheSolve() function

#The below function computes the inverse of the matrix created by the above function makeCacheMatrix.
#The computed inverse matrix values will be cached and returned in the subsequent computations, only
#if the initial matrix values remain unchanged. 

cacheSolve <- function(x, ...) {
  IM <- x$getinverse() 
  if(!is.null(IM)) {
    message("getting cached data")
    return(IM)
  }                        ##returns the inverse IM matrix from cache
  matrix <- x$get()        ##if initial matrix changed, then it get's the new matrix and compute IM() 
  IM <- solve(matrix, ...) ## the solve() function used to compute the inverse of the created matrix 
  x$setinverse(IM)
  IM                       ## returns the inverse matrix  
}

#Testing the created R-Script

MyMatrix<-makeCacheMatrix(matrix(c(100,200,300,400), 2,2)) 
#lexical scoping allows MyMatrix to have a complete copy of the environment for makeCacheMatrix(), 
#including all the objects that are defined within makeCacheMatrix().
cacheSolve(MyMatrix)
#when the cacheSolve() was called for the second time, it returns the value from the cache and  
#avoides the time consuming recomputation.
