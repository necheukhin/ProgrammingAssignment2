makeCacheMatrix <- function(x = matrix()) {
	d <- NULL  
	set <- function (y){
                x <<- y
                d <<- NULL 
			}
		get <- function() x  
		set.inv <- function(solve) d <<- solve
		get.inv <- function() d
		list( set = set , get = get, 
		set.inv =set.inv ,
		get.inv =get.inv)
	
}

cacheSolve <- function(x, ...) {

			d <- x$get.inv() 
			if(!is.null(d)) {
		message("getting cached inverted matrix")
			return(d)
			}
			data <- x$get()
			d <- solve(data, ...)
			x$set.inv(d)
			d
        }
