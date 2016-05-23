##cachematrix.R
## William W Yee, Sunday May 22nd, 2016
## R programming week 3, Coursera 
## Taught by Dr. Roger Peng


#This function is written to calculate the inverse of a matrix and saves
##the result in a cache. The cache can be access if the user attempts to
##recalculate the matrix inverse.

##step 1. set the value of the matrix
##step 2. get the value of the matrix
##step 3. set the value of the inverse
##step 4. get the value of the inverse

makeCacheMatrix <- function (x = matrix()) {
        
        m<- NULL                        ##initialize the cache in this enviro
        set<-function(y){
                x<<-y                   ##assigns the input matrix, g, to x in parent enviro
                m<<-NULL                ##initial cache in the parent enviro
        }
        get<-function() x               ##return the input matrix, x
        setinverse<- function(inverse)  m <<- inverse
                                        ##sets the cache equal to the inverse of x
        getinverse<- function() m       ##returns the cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##The following function calculates the inverse of the special "vector" created 
##with the above function. However, it first checks to see if the inverse
##has been already calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise it caclculates the inverse and sets the
##value using the 'setinverse' function.

cacheSolve <- function (x, ...){
        
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting the cached data")
                return(m)
        }
        data <- x$get()
        m<- solve(data)
        x$setinverse(m)
        m
}
