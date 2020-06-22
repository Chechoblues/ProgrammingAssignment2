#At first, I'm gonna set input n as a matrix, with their respective dimension, from a random numerical values.
#Secondly, I'll set "r" (resoluted value) as NULL, to prevent it's Inverted return by  now
#Lastly, I'll follow the example changing "mean" for "result"
makeCacheMatrix <- function(n = matrix(rnorm(24),6,4)) {
  r <- NULL
  set <- function(p) {
    m <<- p
    r <<- NULL
  }
  get <- function() n
  setResult <- function(solve) r <<- solve
  getResult <- function() r
  list(set = set, get = get,
       setResult = setResult,
       getResult = getResult)
}

#In the second part, I conserve the same changes
cacheSolve <- function(x, ...) {
  r <- m$getResult()
  if(!is.null(r)) {
    message("Getting Resoluted Matrix")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  m$setResult(r)
  r
}