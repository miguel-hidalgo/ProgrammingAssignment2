##
## Function Name: makeCacheMatrix
## Purpose......: Creates a persistent Matrix object for caching
## .............: 
##..............: Also provides property like methods (functions) to store and retreat 
## .............: a previously stored Matrix.
## Returns .....: Returns the list interface to interact with the Matrix, set(), get(),
## .............: getmatrix(), setmatrix()

makeCacheMatrix <- function(x = matrix()) {
      
      ## Define and set to NULL the matrix variable.
      imtx <- NULL
     
       ## Define set function- This function sets/stores the original matrix to be inversed
      set <- function(y) {
        ## Store in a persistent environment the original matrix
        x <<- y
        ## Set to NULL the inversed matrix persistent variable
        imtx <<- NULL
      }
    
      ## Returns the original matrix
      get <- function() x
      
      ## Sets/stores the inversed matrix into the persistent variable
      setmatrix <- function(mtrix) imtx <<- mtrix
      
      ## returns the inversed matrix variable.  If it has not been calculated 
      ## is set to NULL (returns NULL)
      getmatrix <- function() imtx
      
      ## Returns a special list that contains the interface to interact with the CacheMatrix
      ## variables.  It returns the 4 functions to interact with the CacheMatrix
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
  
}



##
## Function Name: cacheSolve
## Purpose......: This function, checks to see if the passed matrix is in the cache object.
## .............: If the matrix is in the cache, get the stored inversed of the matrix 
## .............: and returns it.
## .............: If the matrix has not been cached before, inverses the matrix, stores
## .............: it in the cache and returns the inverse matrix t its caller.
## Return ......: a matrix that is the inverse of 'x'
##
cacheSolve <- function(x, ...) {
 
      ## Call the getmatrix function to get the inversed of the original matrix
      mtx <- x$getmatrix()
  
      ## If the matrix returned equals NULL, then it has not been cached.
      ## If the matrix returned is not equal to NULL, returns the cached inversed matrix
      if(!is.null(mtx)) {
          message("getting cached data")
          ## returning the inversed matrix from cache
          return(mtx)
      } else {
        message("data not in cache")
      }
      ## if this part of the code is executed, it means that
      ## the Matrix has not been inversed.  So, get original matrix
      ## so it can be inverse after.
      data <- x$get()
      
      ## Call the Solve function in R to inverse the Matrix and store it in mtx
      mtx <- solve(data, ...)
      
      ## Set/store the inversed Matrix in the Cache
      x$setmatrix(mtx)
      
      ## return inversed Matrix to be used by the caller of cacheSolve()
      return (mtx)
}

##
## Function Name: cacheTester 
## Purpose......: This function, test the cache functions 
## .............: Creates a squared Matrix from the code in the R help for solve()
## .............: Solves (Inverses the created Matrix) for comparison
## .............: Calls makeCacheMatrix with the square matrix
## .............: calls cacheMatrix with the square matrix message not in cache appears
## .............: calls cacheMatrix a second and third time with the square matrix message getting cache data appears
## Return ......: Output the inversed matrix to screen before cach and after cache with the 
## .............: Messages Data Not in Cache and getting cached data.
##
cacheTester <- function() {
 
  print("Begin Test")
  ## Create square Matrix
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }   
  h8 <- hilbert(8); h8
  ## create and inversed matrix to compare later
  sh8 <-solve(h8)
  ## Create cacheMatrix object
  x<-makeCacheMatrix(h8)
  ## Get the original data from the matrix object
  odata<-x$get()
  ## create a logig array that records all the compared values in the matrices
  lod<-odata==h8
  ## checks that the matrix in the cache is identical to the original passed
  if (all(lod)) print ("TRUE") else print("FALSE")
  
  ## Call cacheSolve to Invert Matrix but check if it has been cached
  im<-cacheSolve(x)
  ## print inverted matrix
  print(im)
  ## Call cacheSolve to Invert Matrix but check if it has been cached
  imc<-cacheSolve(x)
  ## print inverted matrix for visualcomparison.
  print(imc)
  ## Compare inversed matrices and return the logical matrix with all elements set to
  ## TRUE if they are equal
  if (all(im==imc)) print ("TRUE") else print("FALSE")
  print(im==imc)
  ## Call cacheSolve to Invert Matrix but check if it has been cached
  imc2<-cacheSolve(x)
  ## print inverted matrix for visualcomparison.
  print(imc2)
  ## Compare inversed matrices and return the logical matrix with all elements set to
  ## TRUE if they are equal
  if (all(im==imc2)) print ("TRUE") else print("FALSE")
  print(im==imc2)
  print("Test Completed")
  
}


