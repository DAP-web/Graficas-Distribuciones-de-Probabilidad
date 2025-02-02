# Para realizar las distribuones de probabilidad se necesita el paquete ggplot2:
install.packages('ggplot2')
library(ggplot2)

# DISTRIBUCI�N GAMMA: La funci�n de densidad tiene dos par�metros, alfa y beta:

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

# Se crea el gr�fico de la distribuci�n Gamma:
ggplot(data_gamma, aes(x_gamma,y_gamma)) + geom_line(colour = "blue", size = 1) + 
  ggtitle("Funci�n de densidad Gamma", paste("Para Alfa =", toString(alfa_gamma), "y Beta =", toString(beta_gamma))) + 
  labs(x ="x", y="Distribuci�n") + theme(plot.title = element_text(color="blue", face = "bold", hjust=0.5))

# DISTRIBUCI�N EXPONENCIAL: La funci�n de densidad tiene un par�metro Beta:

# Ingrese el valor de beta:
beta_exponencial <- 2
beta_exponencial_reciproco <- 1/beta_exponencial

# Valores de la variable aleatoria continua X:
x_exponencial <- seq(-1, 7.5, length.out=1000)

# Valores de densidad de cada valor de x:
y_exponencial <- dexp(x_exponencial, rate = beta_exponencial_reciproco)

# Valores de la probabilidad para cada valor que X puede tomar:
data_exponencial <- data.frame(x_exponencial, y_exponencial)

# Se crea el gr�fico de la distribuci�n Exponencial:
ggplot(data_exponencial, aes(x_exponencial, y_exponencial)) + geom_line(colour = "blue", size=1)+
  ggtitle("Funci�n de densidad Exponencial", paste("Para Beta =", toString(beta_exponencial))) + 
  labs(x ="x", y="Distribuci�n") + theme(plot.title = element_text(color="blue", face = "bold", hjust=0.5))

