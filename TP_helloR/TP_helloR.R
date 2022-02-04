df = read.csv('nombres-2000-2004.csv')
head(df)
df$nombre = tolower(df$nombre)

nombres_unicos = unique(df$nombre)
length(nombres_unicos)

unicos_anio = tapply(df$nombre, df$anio, unique)

J = matrix(NA, 5, 5)
for (i in 1:dim(J)[1]) {
  for (j in 1:dim(J)[2]) {
    J[i, j] = length(intersect(unicos_anio[[i]], unicos_anio[[j]])) / length(union(unicos_anio[[i]], unicos_anio[[j]]))
  }
}

# outer produce variables conjuntas x,y en todas las combinaciones - se complica por los tipos
# J = outer(unicos_anio, unicos_anio, function(x, y))
# J = outer(1:5, 1:5, function(x, y) length(intersect(unicos_anio[x], unicos_anio[y])) / length(union(unicos_anio[x], unicos_anio[y])))

frecuencias = tapply(df$cantidad, df$nombre, sum)
