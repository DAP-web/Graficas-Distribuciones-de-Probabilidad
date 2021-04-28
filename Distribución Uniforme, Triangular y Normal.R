#Se necesita instalar y cargar el paquete de GGPLOT2
#install.packages("ggplot2")
library(ggplot2)


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
  