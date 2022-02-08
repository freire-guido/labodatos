### PARTE 1

df_nombres = read.csv('nombres-2000-2004.csv')

# A
head(df_nombres) # El dataset tiene las cantidades de nombres que se registraron por anio, ordenados por anio creciente y luego cantidad decreciente
df_nombres$nombre = tolower(df_nombres$nombre)

# B
nombres_unicos = unique(df_nombres$nombre)
length(nombres_unicos)
unicos_anio = tapply(df_nombres$nombre, df_nombres$anio, unique)
# Hay mas de 500 mil nombres unicos, pero cada anio tiene bastante mas de 100 mil nombres unicos - esperaria bastantes repeticiones para cualquier par de anios

# C
jaccard = function(x, y) {
  return(length(intersect(x, y)) / length(union(x, y)))
}

matriz_jaccard = function(x, y = x) {
  J = matrix(NA, 5, 5)
  for (i in 1:dim(J)[1]) {
    for (j in 1:dim(J)[2]) {
      J[i, j] = jaccard(x[[i]], y[[j]])
    }
  }
  return(J)
}

J = matriz_jaccard(unicos_anio); rownames(J) = 2000:2004; colnames(J) = 2000:2004

# aca trate de usar outer para calcular J, tengo entendido que le escupe a la funcion todas las combinaciones entre los dos primeros argumentos,
# pero arma bardo con las dimensiones asi que me di por vencido.
# J = outer(unicos_anio, unicos_anio, jaccard)
# J = outer(1:5, 1:5, function(x, y) jaccard(unicos_anio[x], unicos_anio[y]))

# Los valores de la matriz J (omitiendo la diagonal) estan, en promedio, por debajo de 0.2 "a ojo" hubiese esperado bastante mas solapamiento entre los nombres de un anio y otro
# Si me quedo con la triangular superior veo que al moverme a lo largo de una fila baja el valor del coef. Jaccard. Es decir, Los anios mas cercanos cronologicamente comparten mas nombres.

# D
# Me aprovecho que el dataset esta ordenado decrecientemente para tomar los primeros y ultimos 10 nombres para cada anio.
mas_comunes_anio = tapply(df_nombres$nombre, df_nombres$anio, function(x) x[1:10])
menos_comunes_anio = tapply(df_nombres$nombre, df_nombres$anio, function(x) x[length(x) - 10: length(x)])

# Los nombres mas comunes son bastante cortos, una o dos palabras - los menos comunes tienen hasta 5 palabras.

J_mas_comunes = matriz_jaccard(mas_comunes_anio)
J_menos_comunes = matriz_jaccard(menos_comunes_anio)

# Haciendo un analisis parecido al del item C, veo que los coef. Jaccard para los nombres MAS comunes son bastante mas elevados que los de los nombres MENOS comunes.

# E
frecuencias_nombre = aggregate(cantidad ~ nombre, data = df_nombres, sum)

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
      x = da_el_paso(x) # medio peligroso armar un while que depende de una variable aleatoria (podria colgarse indefinidamente?)
      pasos[length(pasos)] = pasos[length(pasos)] + 1
    }
    bordes = c(bordes, x >= T1)
  }
  return(cbind(pasos, bordes))
}

# La funcion camina simula n caminatas y devuelve la cantidad de pasos y el borde final para cada una, en formato de matriz

# D
df_caminatas = as.data.frame(camina(0, -1, 10, 1000))
promedios_borde = tapply(df_caminatas$pasos, df_caminatas$bordes, mean)

# Las caminatas que terminan en el borde mas lejano a x0 tardan, mas veces que el resto

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
