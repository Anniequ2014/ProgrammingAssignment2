## This function returns a list of four functions that will set a matrix, get the value of the matrix, set the value of the 
## matrix' inverse and get the value of the inverse matrix.

## Each element in the list returned by makeCacheMatrix is a function defined within the main function makeCacheMatrix. 

makeCacheMatrix <- function(x = matrix()) {
        Inv<-NULL
        set<-function(y){
                x<<-y
                Inv<<-NULL
    }
        get<-function() x
        setInverse<-function(Inverse)Inv<<-Inverse
        getInverse<-function() Inv
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes or retrieves the stored inverse matrix of the matrix returned by function makeCacheMatrix

cacheSolve <- function(x=makeCacheMatrix(), ...) {
        Inv<-x$getInverse()
        if(!is.null(Inv)){
	        message("getting cached data")
	        return(Inv)
	}
	data<-x$get()
	Inv<-solve(data)
	x$setInverse(Inv)
	Inv

}
