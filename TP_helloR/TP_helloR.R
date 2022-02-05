### PARTE 1

df_nombres = read.csv('nombres-2000-2004.csv')

# A
head(df_nombres)
df_nombres$nombre = tolower(df_nombres$nombre)

# B
nombres_unicos = unique(df_nombres$nombre)
length(nombres_unicos)
unicos_anio = tapply(df_nombres$nombre, df_nombres$anio, unique)

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

# outer produce variables conjuntas x,y en todas las combinaciones - se complica por las dimensiones
# J = outer(unicos_anio, unicos_anio, jaccard)
# J = outer(1:5, 1:5, function(x, y) jaccard(unicos_anio[x], unicos_anio[y]))

# D
frecuencias_anio = tapply(df_nombres$nombre, df_nombres$anio, function(x) x[1:10]) # me aprovecho de que ya estan ordenados por cantidad decreciente

# E
frecuencias_nombre = tapply(df_nombres$cantidad, df_nombres$nombre, sum)

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
  pasos = NULL
  bordes = NULL
  for (i in 1:n) {
    pasos = c(pasos, 0)
    x = x0
    while (esta_entre(x, T0, T1)) {
      x = da_el_paso(x)
      pasos[length(pasos)] = pasos[length(pasos)] + 1
    }
    bordes = c(bordes, x >= T1)
  }
  return(cbind(pasos, bordes))
}

# D
df_caminatas = as.data.frame(camina(0, -1, 10, 1000))
promedios_borde = tapply(df_caminatas$pasos, df_caminatas$bordes, mean)

# E
calcular_recorrido = function(x0, T0, T1) {
  while (esta_entre(x0[length(x0)], T0, T1)) {
    x0 = c(x0, da_el_paso(x0[length(x0)]))
  }
  return(x0)
}

# BONUS TRACK !
plotear_recorrido = function(x0, T0, T1, n=1) {
  plot(calcular_recorrido(x0, T0, T1), type='l', xlim=c(1,400), ylim=c(T0, T1), main='Recorridos de n caminatas aleatorias', xlab='paso', ylab='valor')
  abline(h=T0); abline(h=T1)
  for (i in 1:n-1) { # (?) plotea uno de mas porque i in 1:n-1 nunca hace 0 iteraciones
    lines(calcular_recorrido(x0, T0, T1))
  }
}
plotear_recorrido(0, -1, 10, 100)
