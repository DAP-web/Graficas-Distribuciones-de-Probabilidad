#Se necesita instalar y cargar el paquete de GGPLOT2 y RESHAPE2
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)


#DISTRIBUCION UNIFORME: Recibe dos valores, alfa y beta
#dunif(valores, min = 'minimo', max = 'maximo')

#Ingrese el valor de alfa de su Distribucion Uniforme en alfaU
alfaU <- 0

#Ingrese el valor de beta de su Distribucion Uniforme en betaU
betaU <- 4

#Grafica de la DISTRIBUCION UNIFORME
curve(dunif(x, min = alfaU, max = betaU), 
      from = (alfaU - 3), to = (betaU + 3),
      n = 100000,
      add = FALSE,
      main = paste("DISTRIBUCION UNIFORME \n CON ALFA =", 
                   alfaU, "Y BETA =", betaU, sep = " "),
      xlab = "x", ylab = "f(x)",
      col = "blue",
      lty = 1, lwd = 3,
      font.lab = 2,
      font = 2)
#Linea horizontal que identifica el maximo valor de la distribucion
abline(h = (1/(betaU - alfaU)), lty = 2, col = "blue", lwd = 2)



#DISTRIBUCION TRIANGULAR: Recibe tres valores, un valor minimo (a)
# un valor maximo (b) y el valor de la moda


#Ingrese el valor del minimo de su DIST. Triangular en aMinimT:
aMinimT <- 2

#Ingrese el valor del maximo de su DIST. Triangular en maximT:
bMaximT <- 9

#Ingrese el valor de la moda de su DIST. Triangular en modaT:
modaT <- 3.5


#Vectores con los valores de la var. independiente X que se evaluaran:

# xValuesT1 se define entre el valor minimo y la moda
xValuesT1 <- seq(from = aMinimT, to = modaT, length.out = 1000)

# xValuesT2 se define entre el valor de la moda y el maximo
xValuesT2 <- seq(from = modaT, to = bMaximT, length.out = 1000)

# xValuesT3 se define antes del valor minimo como 0
xValuesT3 <- seq(from = (aMinimT - 1), to = aMinimT, length.out = 1000)

# xValuesT4 se define despues del valor maximo como 0
xValuesT4 <- seq(from = bMaximT, to = (bMaximT + 1), length.out = 1000)



#Dataset con los valores correspondientes a la primera porcion de la dist.
datosT1 <- data.frame(
  xTriang1 = xValuesT1,
  yTriang1 = (2)*(xValuesT1 - aMinimT)/((bMaximT - aMinimT)*(modaT - aMinimT))
)

#Dataset con los valores correspondientes a la segunda porcion de la dist.
datosT2 <- data.frame(
  xTriang2 = xValuesT2,
  yTriang2 = (2)*(bMaximT - xValuesT2)/((bMaximT - aMinimT)*(bMaximT - modaT))
)

#Dataset con los valores de probabilidad afuera de los limites
datosT3 <- data.frame(
  xTriang3 = xValuesT3,
  yTriang3 = xValuesT3*0
)

#Dataset con los valores de probabilidad afuera de los limites
datosT4 <- data.frame(
  xTriang4 = xValuesT4,
  yTriang4 = xValuesT4*0
)


#GRAFICA DE LA DISTRIBUCION TRIANGULAR
ggplot(datosT1, aes(x = xTriang1, y = yTriang1)) + 
  geom_line(size = 1.5, colour = "blue") + 
  geom_line(size = 1.5, colour = "blue", data = datosT2, 
            aes(x = xTriang2, y = yTriang2, colour = "blue")) +
  geom_line(size = 1.5, colour = "blue", data = datosT3, 
            aes(x = xTriang3, y = yTriang3, colour = "blue")) +
  geom_line(size = 1.5, colour = "blue", data = datosT4, 
            aes(x = xTriang4, y = yTriang4, colour = "blue")) +
  geom_vline(xintercept = modaT, lty = 2, colour = "blue", size = 1) + 
  #Linea vertical que identifica el valor maximo de la DIST. TRIANGULAR
  geom_hline(yintercept = 0, lty = 1, colour = "black") +
  labs(x = "\n x", y = "f(x)", 
       title = paste("GRAFICA DE DIST. TRIANGULAR \n CON MINIMO (a) =",
                     aMinimT, "| MAXIMO (b) =", bMaximT, "| MODA =", 
                     modaT, sep = " ")) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.2)), 
        legend.position = "none")



#DISTRIBUCION NORMAL: Recibe 2 valores, la media y la desviacion estandar
#dnorm(valores, mean = 'media', sd = 'desviacionEstandar')

#Ingrese el valor de la media de su DIST. Normal en mediaNorm:
mediaNorm <- 3

#Ingrese el valor de la DESV. EStandar de su DIST. Nomral en desvestNorm:
desvestNorm <- 0.5

#GRAFICA DE LA DISTRIBUCION NORMAL
curve(dnorm(x, mean = mediaNorm, sd = desvestNorm),
      from = mediaNorm - (3 * desvestNorm),
      to = mediaNorm + (3 * desvestNorm),
      n = 10000,
      add = FALSE,
      ylab = "f(x)", xlab = "\n x",
      main = paste("GRAFICA DE DIST. NORMAL \n CON MEDIA =", 
                   mediaNorm, "Y DESVEST =", desvestNorm, sep = " "),
      lty = 1, lwd = 3,
      font.lab = 2,
      font = 2,
      col = "blue")
#Linea vertical que identifica el valor de la media de la DIST NORMAL
abline(v = mediaNorm, col = "blue", lty = 2, lwd = 2)



# DISTRIBUCIÓN GAMMA: La función de densidad tiene dos parámetros, alfa y beta
#dgamma(valores, shape = 'valorDeAlfa', rate = 'valorDeBeta')


# Ingrese el valor de alpha:
alfa_gamma <- 2

# Ingrese el valor de beta:
beta_gamma <- 3


# Valores de la variable aleatoria continua X:
x_gamma <- seq(-1,7.5, length.out=1000)

# Valores de densidad de cada valor de x:
y_gamma <- dgamma(x_gamma, shape = alfa_gamma, rate = beta_gamma)


# Valores de la probabilidad para cada varor que X puede tomar:
data_gamma <- data.frame(x_gamma, y_gamma)


#GRAFICA DE LA DISTRIBUCION GAMMA:
ggplot(data_gamma, aes(x_gamma,y_gamma)) + 
  xlim(c(0, 7)) + ylim(c(0, 1)) +
  geom_line(colour = "blue", size = 1) + 
  ggtitle("Función de densidad Gamma", 
          paste("Para Alfa =", toString(alfa_gamma), 
                "y Beta =", toString(beta_gamma))) + 
  labs(x ="x", y="Distribución") + 
  theme(plot.title = element_text(color="blue", face = "bold", hjust=0.5))



# DISTRIBUCIÓN EXPONENCIAL: La función de densidad tiene un parámetro Beta:
#dexp(valores, rate = 'ReciprocoBeta')


# Ingrese el valor de beta:
beta_exponencial <- 2
beta_exponencial_reciproco <- 1/beta_exponencial


# Valores de la variable aleatoria continua X:
x_exponencial <- seq(-1, 7.5, length.out=1000)

# Valores de densidad de cada valor de x:
y_exponencial <- dexp(x_exponencial, rate = beta_exponencial_reciproco)


# Valores de la probabilidad para cada valor que X puede tomar:
data_exponencial <- data.frame(x_exponencial, y_exponencial)


#GRAFICA DE LA DISTRIBUCION EXPONENCIAL
ggplot(data_exponencial, aes(x_exponencial, y_exponencial)) + 
  geom_line(colour = "blue", size=1)+
  ggtitle("Función de densidad Exponencial", 
          paste("Para Beta =", toString(beta_exponencial))) + 
  labs(x ="x", y="Distribución") + 
  theme(plot.title = element_text(color="blue", face = "bold", hjust=0.5))



#DISTRIBUCION BETA: Recibe dos parametros, alfa y beta
#dbeta(valores, shape1 = 'alfa', shape2 = 'beta')


#Insertar valores de alfa en alpha_b:
alpha_b <- 3

#Insertar valores de beta en beta_b:
beta_b <- 2


#Longitud de la gráfica en x
x <- seq(0, 1, length.out = 10000)


#Distribución dbeta:
distribucion <- dbeta(x, shape1 = alpha_b, shape2 = beta_b)

#Ordenar la tabla de los datos
distribucion <- melt(distribucion,x)


#GRAFICA DE LA DISTRIBUCION BETA
g <- ggplot(distribucion, aes(x,value))

g + geom_line(colour = "blue") + labs(title="Distribución Beta") + 
  labs(x="Probabilidad", y="Valores") +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.2)), 
        legend.position = "none")



#DISTRIBUCION WEIBULL: Recibe dos parametros, alfa y beta
#dweibull(valores, shape = 'alfa', scale = 'beta', log = FALSE)


#Determinar el valor de alfa en alpha_d:
alpha_d <- 2

#Determinar el valor de beta en beta_d:
beta_d <- 1


#GRAFICA DE LA DISTRIBUCION WEIBULL
curve(dweibull(x, shape = alpha_d, scale = beta_d, log = FALSE), 
      ylab = 'densidad', 
      from = 0, to = 10, 
      n=10000, 
      col = "blue", 
      main = paste("Distribución Weibull \n  con alfa =", 
                   alpha_d, "y beta =", beta_d, sep = " "))



#DISTRIBUCION LOGNORMAL: Recibe dos parametros, alfa y beta
#dlnorm(valores, meanlog = 'mediaValue', sdlog = 'desvestValue')


#Variables necesarias:
#Ingresar el valor de alfa en a:
a <- 0

#Ingresar el valor de beta en beta_d:
b <- 1


#Determinar los valores de la media y desvest 
medialn <- exp(a + ((b^2)/2))
desln <- sqrt(exp(2*a + (b^2)) * (exp(b^2) - 1))


#GRAFICA DE LA FUNCION LOGNORMAL
curve(dlnorm(x, meanlog = medialn, sdlog = desln), 
      from = 0, to = 14, 
      n = 10000, 
      add = FALSE, 
      xlab = "x", ylab = "f(x)",
      col = "Blue", 
      main=paste("Distribucion LogNormal \n para alfa = ", a ,"Y beta = ", b))



#DISTRIBUCION ERLANG: Recibe dos parametros, alfa (valor entero) y lambda
#dgamma(valores, valorEnteroAlfa, rate = 'valorBeta')


#Se especifican las variables 
#Ingresar el valor de alfa en alfa:
alfa <- 2

#Ingresar el valor de lambda en lambda:
lambda <- 2


#Sacar beta, media y varianza 
beta <- 1/lambda
media <- alfa*lambda
std <- sqrt(alfa*beta^2)


#GRAFICA DE DISTRIBUCION ERLANG
curve(dgamma(x, alfa, rate = 1/beta), 
      from = 0, to = 14, 
      n= 10000, 
      add= FALSE, 
      xlab = "x", ylab = "f(x)",
      col = "Blue", 
      main=paste("Distribución Erlang \n para alfa = ", 
                 alfa ,"Y lambda = ", lambda)
      )
