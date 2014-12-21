## This function is used to pass a vector, this vector creates an object that stores vecor value and cached value. 
##It is initiallyset to null

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
	#This fucntion returns the value of original vector
        get <- function() 
	{
		x
	}
	##setter method, called very first time from cacheSolve, subsequent calls with same vector will not be calling this 
        setinverse <- function(inverse) 
	{
		i <<- solve(x)
	}
	##Called after first call returns cached value
        getinverse <- function()
	{	
		 i
	}
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}



## When called gets the inverse  from the object x$getinverse (),
## If value present message getting cached data will be printed
## Otherwise inverse is done and displayed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse () #gets the value
	## If not null gets cached value
	
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	## Calcutale the inverse of matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
