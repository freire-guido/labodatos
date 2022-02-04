2**3 == 2^3
class('e' == 3)
(TRUE || FALSE) == (TRUE || NA)
(TRUE || FALSE) == (FALSE | NA) # hay cortocircuito
x=19
if (x<-3) x # asigna!!!
class(~TRUE == FALSE)
?formula
M = array(c(1:10, 10:1)); dim(M) <- c(10,2)
class(M) # array con dimension es igual a matriz
Sm = 1:25; dim(Sm) <- c(5,5)
attach(svd(Sm)) # (no usarlo asi lol)
?attach()
search()
detach(svd(Sm))
search()
edificio = 0 + Inf
m = rep(1, Inf); dim(m) = 3 # no hay objetos de dimension infinita :(
lista = list('uno'=1,'dos'=2,'tres'=3)
lista[1] # las listas son vectores de vectores
lista[[1]]
lista$uno
keyrepe = list('uno'= 1, 'uno'=2)
keyrepe$uno # permite nombres repetidos pero devuelve la primer ocurrencia
matriz = matrix(1:20, 10, 2)
matrindice = cbind(1:10, 1:2) # puedo indexar con numeros, vectores y matrices
matriz[matrindice]
1 == 1i
1 == i # i no esta reservado
class(1i)

###

attach(faithful)
plot(eruptions, main='amantes del diseño grafico')
recorte = eruptions[2*1:100]; dim(recorte) = c(10, 10)
eruptions

###

d <- outer(0:9, 0:9)
fr <- table(outer(d, d, "-"))
plot(fr, xlab="Determinant", ylab="Frequency")
