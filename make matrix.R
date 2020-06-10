makeCacheMatrix <- function(x = matrix()) {
  icd <- NULL
  set <- function(y) {
    x <<- y
    icd <<- NULL
  }
  get <- function() x
  seticderse <- function(icderse) icd <<- icderse
  geticderse <- function() icd
  list(set = set,
       get = get,
       seticderse = seticderse,
       geticderse = geticderse)
}
cacheicd <- function(x, ...) {
  
  s <- x$geticd()
  
  if(!is.null(s)) {
    
    message("getting inversed matrix")
    
    return(icd)
    
  }
  
  data <- x$get()
  
  s <- icd(data, ...)
  
  x$seticd(icd)
  
   return(icd) 
  
}