########################################################################
##   C  A  C  H  E     M  A  T  R  I  X
##   A  S  S  I  G  M  E  N  T       #2
########################################################################

# makeCacheMatrix contains all functions that make the interface
# between the user, the cache and the cacheSolve function
# in order to load the inverse matrix to the cache

makeCacheMatrix <- function(x = matrix()) {
  # inicializacion de variable hacia cache
  matrixcache <- NULL
  
  # Matriz al working environment
    set <- function(y) {
    x <<- y
    matrixcache <<- NULL
  }
  
  # Obtiene la matriz
  get <- function() x
  
  # Almacena la inversion de la matriz en el cache
  setMatrix <- function(matrizinvertida) matrixcache <<- matrizinvertida
  
  # Retorna la matriz inversa si existe en cache
  getInverse <- function() matrixcache
  

  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


# cacheSolve is a function that calculate the inverse matrix if not exist
# if exist, it return the matrix from cache
cacheSolve <- function(x, ...) {
  ## consulta si existe inversa en el cache
  matrixcache <- x$getInverse()
  
 # si no existe la matriz inversa entonces procede a calcularla (if)
  if (is.null(matrixcache)) {
    #asigna a la variable matrix el valor que tiene actualmente
    matrix <- x$get()
    # calcula la matriz inversa 
    matrixcache <- solve(matrix, ...)
    # lleva la matriz al cache
    x$setMatrix(matrixcache)
    return (matrixcache)
  }
# si existe la matriz inversa consulta el cache
  else {
  message("Desde Cache")
  return(matrixcache)
  }
}