#Distribución LogNormal 
#Variables que se necesitan
a <- 0
b <- 1

#Convertir a la media y desviación ln 
medialn <- exp(a + ((b^2)/2))
desln <- sqrt(exp(2*a + (b^2)) * (exp(b^2)-1))

#Definición de la variable X en LogNormal
xvaloresln <- seq(0, 12, length.out=100)

#Hacer un dataframe para la gráfica
DatosLNormal <- data.frame(
xln =xvaloresln,
yln = dlnorm(xvaloresln, meanlog = medialn, sdlog = desln)
)

#Gráfica en Curve 
curve(dlnorm(x, meanlog = medialn, sdlog = desln), 
      from = 0, to = 14, 
      n = 10000, 
      add = FALSE, 
      xlab = "x", ylab = "f(x)",
      col = "Blue", 
      main=paste("Distribucion LogNormal \n para alfa = ", a ,"Y beta = ", b))



#Distribucion Erlang 
#Se especifican las variables 
alfa <- 2
lambda <- 2

#Sacar beta, media y varianza 
beta <- 1/lambda
media <- alfa*lambda
std <- sqrt(alfa*beta^2)

#Definicion de la variable x en Erlang
xvaloresErlang = seq(0, media + 4*std,  0.02)

#Hacer un dataframe 
DatosErlang <- data.frame(
  xErlang = xvaloresErlang,
  yErlang = dgamma(xvaloresErlang, alfa, rate = 1/beta)
)

#Gráfica en Curve 
curve(dgamma(x, alfa, rate = 1/beta), from = 0, to = 14, n= 10000, add= FALSE, xlab = "x", ylab = "f(x)",
      col = "Blue", main=paste("Distribución Erlang \n para alfa = ", alfa ,"Y lambda = ", lambda))

