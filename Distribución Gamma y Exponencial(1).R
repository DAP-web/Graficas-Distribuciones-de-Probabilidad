# Para realizar las distribuones de probabilidad se necesita el paquete ggplot2:
install.packages('ggplot2')
library(ggplot2)

# DISTRIBUCIÓN GAMMA: La función de densidad tiene dos parámetros, alfa y beta:

# Ingrese el valor de alpha:
alfa_gamma <- 2

# Ingrese el valor de beta:
beta_gamma <- 1

# Valores de la variable aleatoria continua X:
x_gamma <- seq(-1,7.5, length.out=1000)

# Valores de densidad de cada valor de x:
y_gamma <- dgamma(x_gamma, shape = alfa_gamma, rate = beta_gamma)

# Valores de la probabilidad para cada varor que X puede tomar:
data_gamma <- data.frame(x_gamma, y_gamma)

# Se crea el gráfico de la distribución Gamma:
ggplot(data_gamma, aes(x_gamma,y_gamma)) + geom_line(colour = "blue", size = 1) + 
  ggtitle("Función de densidad Gamma", paste("Para Alfa =", toString(alfa_gamma), "y Beta =", toString(beta_gamma))) + 
  labs(x ="x", y="Distribución") + theme(plot.title = element_text(color="blue", face = "bold", hjust=0.5))

# DISTRIBUCIÓN EXPONENCIAL: La función de densidad tiene un parámetro Beta:

# Ingrese el valor de beta:
beta_exponencial <- 2
beta_exponencial_reciproco <- 1/beta_exponencial

# Valores de la variable aleatoria continua X:
x_exponencial <- seq(-1, 7.5, length.out=1000)

# Valores de densidad de cada valor de x:
y_exponencial <- dexp(x_exponencial, rate = beta_exponencial_reciproco)

# Valores de la probabilidad para cada valor que X puede tomar:
data_exponencial <- data.frame(x_exponencial, y_exponencial)

# Se crea el gráfico de la distribución Exponencial:
ggplot(data_exponencial, aes(x_exponencial, y_exponencial)) + geom_line(colour = "blue", size=1)+
  ggtitle("Función de densidad Exponencial", paste("Para Beta =", toString(beta_exponencial))) + 
  labs(x ="x", y="Distribución") + theme(plot.title = element_text(color="blue", face = "bold", hjust=0.5))

