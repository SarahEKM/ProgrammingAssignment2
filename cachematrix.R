## Put comments here that give an overall description of what your
## functions do. These functions allow the inverse of a matrix to be stored without the need to re-calcualte it. Please see below for comments on each element of the code 

## An R object storing the matrix and the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # creates an empty object for use later in the code
  set <- function(aVector){
    x <<- aVector #aVector is a numeric vector and will be assigned to the parent environment
    m <<- NULL #assigns NULL to the 'm' in the parent environment. It also overwrites any pre-existing value of 'm' produced by cacheSolve and assigns NULL to it
  }
  get <- function()x #x is the value the 'getter' gets. x sits outside the bracket because we want it to pull from the parent environment not from within the function defined by 'get'
  setCacheInv <- function(cacheinv) { #is a 'setter' part of the function to cache the inverse. 
    m <<- cacheinv #this says that whatever is calculated as the inverse of the matrix to place that value within the parent environment under the object'm'  
    }
  getCacheInv <- function()m #this looks to 'get' the value of 'm' from the parent environment
  list(set = set, get = get, #the list function creates an object in the parent environment that contains each element of the function as a named value. Allows the cacheColve function to pull with the $ operator
       setCacheInv = setCacheInv,
       getCacheInv = getCacheInv)
}


## Uses an argument output of makeCacheMatrix to then check if the inverse has been cached. If it has it will return the value. If it hasn't been, cacheSolve will then calculate it and store it in makeCacheMatrix under the setCacheInv part

cacheSolve <- function(makematrix.object, ...) {# creation of the formal argument makematrix.object
       mm <- makematrix.object$getCacheInv() # saves the pre-exisiting value of getCacheInv to 'mm'. 
                  #If it is the 1st time the function has been run with a particular matrix as x in makeCacheMatrix the value of 'mm' will be NULL. 
                  #If it has been run previously and there is something already cached it will assign the cached value to 'mm'
       if(!is.null(mm)){ #if the 'if' statement is TRUE (ie. a value other than NULL in 'mm', the message and the value of 'mm' will be returned. If the 'if' statement is FALSE (ie. mm = NULL) the code continues on below to calculate the inverse
         message("getting cached data")
       return (mm)
       }
       data <- makematrix.object$get() #assigns the value of the 'get' element in makeCacheMatrix (which pulls directly from x in the parent environment) to 'data'
       mm <- solve(data, ...) #only assigning 'data' to the solve function (ie. not providing a 'b' value in the arguments list) tells the function we want it to calculate the inverse as store it as 'mm'
       makematrix.object$setCacheInv(mm) #here the function says to rewrite whatever is present in 'setCacheInv' in makeCacheMatrix with the matrix value in 'mm'
       mm #because we want to see the value on the initial solve we ask it to print here too. 
}


#Test cases: 
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
