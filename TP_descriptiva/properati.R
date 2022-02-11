datos = read.csv('ar_properties.csv', stringsAsFactors = FALSE)
head(datos)

datos_caba = datos[datos$l2 == 'Capital Federal',]
datos_caba = datos_caba[datos_caba$currency == 'USD' & datos_caba$operation_type == 'Venta' & !is.na(datos_caba$currency),]
any(is.na(datos_caba$currency)) # Todos tienen precio en dolares

datos_caba = datos_caba[!is.na(datos_caba$surface_total) & !is.na(datos_caba$rooms) & !is.na(datos_caba$property_type),]

# Veo media mediana moda
mean(datos_caba$price)
median(datos_caba$price)
which.max(table(datos_caba$price))
# Mas medidas resumen
sd(datos_caba$price)
IQR(datos_caba$price)
fivenum(datos_caba$price)
summary(datos_caba$price)

hist(datos_caba$price, breaks=500, xlim = c(0,1.5e06))
abline(v=mean(datos_caba$price), col='red')
abline(v=median(datos_caba$price), col='blue')

boxplot(datos_caba$price, outline = FALSE)
barplot(sort(table(datos_caba$property_type), decreasing = TRUE))

boxplot(price ~ property_type, data = datos_caba, outline = FALSE) # Boxplot pierde informacion de tamaño

propiedades = unique(datos_caba$property_type)
colores = rainbow(length(propiedades))
names(colores) = propiedades
plot(price ~ surface_total, data = datos_caba, log = 'xy', col = colores[datos_caba$property_type]) # es lineal en dos ejes log: "polinomica"
abline(h=mean(datos_caba$price), v=mean(datos_caba$surface_total))
legend('right', legend = propiedades, col = colores, pch=1)
