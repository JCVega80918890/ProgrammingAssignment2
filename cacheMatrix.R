## Función que guarda la matrix en cache
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Función resuelve inversa de la matrix en cache
cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if (!is.null(inver)) {
    message("Datos guardados en Cache")
    return(inver)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinverse(inver)
  inversa
}

# Ejemplo de cálculo
M <- matrix(10:13,2,2)
M
Minv <- makeCacheMatrix(M)
cacheSolve(Minv) 
