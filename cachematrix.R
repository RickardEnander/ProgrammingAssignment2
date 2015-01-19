##
## The makeCacheMatrix function acts like a constructor
## in an object oriented. The "object" has three "methods"
##   set(matrix)    - sets matrix
##   get()          - gets previously set matrix
##   setinverse     - sets the member cachedInvesre
##   getinverse     - gets the meber cachedInverse
##
## The function is "copied" from the assignement with some
## naming alterations in order to make it easier to
## understand.
##

makeCacheMatrix <- function(x = matrix()) {
     cachedInverse <- NULL
     set <- function(y) {
            x <<- y
            cachedInverse <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) cachedInverse <<- inverse
     getinverse <- function() cachedInverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## 
## The cacheSolved function takes a "matrix object" created 
## by makeCacheMatrix as argument and returns the
## inverse of that matrix. First time the function actually
## calculates the inverse. Following time the function
## instead returns the cached inverse (from the first call)
##

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
           message("getting cached data")
           return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

##
## function testCacheMatrix is just a utility funktion
## to test the implementation. You could call it a
## unit test.
## Comments in message statements inicates what is expected
##

testCachedMatrix <- function() {
  matrix1   <- matrix(rnorm(40000),200,200)
  cachedM_1 <- makeCacheMatrix( matrix1 )
  message( "COMMENT : Expected class for cachedM_1 is list : ")
  print( class( cachedM_1 ) )

  invM_1_a  <- cacheSolve( cachedM_1 )
  invM_1_b  <- cacheSolve( cachedM_1 )
  message("COMMENT Should indicated <getting cached data>")
  isAlike = identical(  invM_1_a,  invM_1_b )
  message("COMMENT : The two inverted matrices should be identical ") 
  print(isAlike)

  cachedM_1$set( matrix1 )
  invM_2 <- cachedM_1$getinverse()
  print("COMMENT : cached  inverse should be NULL") 
  invM_2
  
  
}

