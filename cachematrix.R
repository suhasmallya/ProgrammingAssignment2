## cachematrix.R - contains two functions: makeCacheMatrix() and cacheSolve()
##
## makeCacheMatrix() creates a special matrix capable of caching its inverse
## cacheSolve() returns the inverse of a matrix - either by retrieving from cache or computing it

## makeCacheMatrix
## pmatrix: matrix parameter passed to the main function 

makeCacheMatrix <- function(pmatrix = matrix()) {
    
  matinverse <- NULL
    
    setmatrix<-function(y){
      pmatrix<<-y
      matinverse<<-NULL
    }
    
    #returns the original matrix 'pmatrix'
    getmatrix<-function() pmatrix
  
    #do not call directly
    setinverse <- function(inv) matinverse <<- inv
  
    #get the inverse of the matrix 'pmatrix'
    #the inverse is held in the object matinverse
    getinverse <- function() matinverse
  
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve
## Returns the inverse of the matrix received by the object 'pmatrix'
## in the above function makeMatrix
##
## Uses the built-in function solve() to compute the inverse

cacheSolve <- function(pmatrix) {
        ## Return a matrix that is the inverse of 'x'
  
    ##Retrieve inverse, if already computed   
    matinverse <-pmatrix$getinverse()
    
    ##Check if null; if not return cached inverse
    if(!is.null(matinverse)){
      
        message ("Retrieving cached inverse")
        matinverse
        return(matinverse)
    }
    
    mymatrix <- pmatrix$getmatrix()
    mymatrix
    
    
    ##Directly calling solve
    ##assumed that mymatrix is always invertible
    
    matinverse<-solve(mymatrix)
    
    ##Cache the inverse
    pmatrix$setinverse(matinverse)
    
    ##Return inverse after computing
    matinverse
}
