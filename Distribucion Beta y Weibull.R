#Gráfico para distribución Beta
#install.packages("reshape2")
#install.packages("ggplot2")
library(ggplot2)

#insertar valores de alpha
alpha_b <- 3
#insertar valores de beta
beta_b <- 2
#longitud de la gráfica en x
x <- seq(0,1,length.out=10000)
#distribución de beta
distribucion<- dbeta(x,shape1=alpha_b,shape2=beta_b)
distribucion
library(reshape2)
#ordenar la tabla de los datos
distribucion <- melt(distribucion,x)
distribucion
#gráfico 
g <- ggplot(distribucion, aes(x,value))
g+geom_line(colour = "blue") + labs(title="Distribución Beta") + labs(x="Probabilidad", y="Valores") +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.2)), 
        legend.position = "none")

#Distribución Weibull
#Determinar alpha
alpha_d <- 2
#Determinar beta
beta_d <- 1
#Gráfico
curve(dweibull(x, shape = alpha_d, scale = beta_d, log = FALSE), ylab = 'densidad', 
      from = 0, to = 10, n=10000, col = "blue", 
      main = paste("Distribución Weibull \n  con alfa =", alpha_d, "y beta =", beta_d, sep = " "),)