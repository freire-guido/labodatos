datos = read.csv('datos_coto2016.csv'); attach(datos)
summary(datos)
hist(price)
datos[which.max(price),]

categorias = unique(categories)     
secos = grep('Secos', categorias, value=TRUE)

prod_secos = datos[categories %in% secos,]

# TODO: IMPLEMENTAR SIN BUCLE FOR
# rango_secos = sapply(categorias, function(cat) range(prod_secos[prod_secos$categories == cat,]$price))
# prod_secos[prod_secos$categories == 'Alimentos Secos,Galletitas',]$price

rango_secos = matrix(NA, length(secos), 2)

for (i in 1:length(secos)) {
  seco = secos[i]
  rango_secos[i,] = range(prod_secos[prod_secos$categories == seco,]$price)
}

rownames(rango_secos) = secos

precio_carrito = function(nombres, cantidad, datos) {
  precio = 0
  for (i in 1:length(nombres)) {
    nombre = nombres[i]
    precio = precio + cuanto_cuesta(nombre, datos)*cantidad
  }
  return(precio)
}

cuanto_cuesta = function(nombre, datos) {
  return(datos$price[datos$name == nombre])
}

elige_productos = function(n, datos) {
  return(sample(name, n))
}

compra_n = function(n, datos) {
  return(precio_carrito(elige_productos(n, datos), rep(1, n), datos))
}


