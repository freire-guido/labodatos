### PARTE 1

df = read.csv('nombres-2000-2004.csv')

# A
head(df)
df$nombre = tolower(df$nombre)

# B
nombres_unicos = unique(df$nombre)
length(nombres_unicos)
unicos_anio = tapply(df$nombre, df$anio, unique)

# C
jaccard = function(x, y) {
  return(length(intersect(x, y)) / length(union(x, y)))
}

J = matrix(NA, 5, 5)
for (i in 1:dim(J)[1]) {
  for (j in 1:dim(J)[2]) {
    J[i, j] = jaccard(unicos_anio[[i]], unicos_anio[[j]])
  }
}
rownames(J) = 2000:2004; colnames(J) = 2000:2004

# outer produce variables conjuntas x,y en todas las combinaciones - se complica por los tipos
# J = outer(unicos_anio, unicos_anio, jaccard)
# J = outer(1:5, 1:5, function(x, y) jaccard(unicos_anio[x], unicos_anio[y]))

# D
frecuencias_anio = tapply(df$nombre, df$anio, function(x) x[1:10]) # me aprovecho de que ya estan ordenados por cantidad decreciente

# E
frecuencias_nombre = tapply(df$cantidad, df$nombre, sum)

### PARTE 2

# A
da_el_paso = function(x0) {
  return(x0 + runif(length(x0), -1, 1))
}

# B
esta_entre = function(x0, T0, T1) {
  return(T0 <= x0 && x0 <= T1)
}

# C
camina = function(x0, T0, T1, n = 1) {
  suma_pasos = 0
  suma_bordes = 0
  for (i in 1:n) {
    pasos = 0
    while (esta_entre(x0, T0, T1)) {
      x0 = da_el_paso(x0)
      pasos = pasos + 1
    }
    suma_pasos = suma_pasos + pasos
    suma_bordes = suma_bordes + (x0 >= T1)
    print(c(pasos, suma_pasos))
    print(suma_bordes)
  }
  return(c(suma_pasos, suma_bordes))
}