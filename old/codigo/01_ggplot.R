# https://r-graphics.org/
# http://mfviz.com/r-image-art/ 

library(tidyverse) #incluye paquete ggplot2
iris #data que viene en R

#capa data
ggplot(data = iris)

#capa aestheric ----
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) 

# variables, mínimo hay que definir dos, x e y
# hay más: "colour" y "fill" (de agrupación), "size" (tamaño letras), "alpha" (transparencia),
# "linetype", "labels" (etiquetas, una variable), "shape" (círculos, triángulos, etc)

# capa geometrías ----
# Hay más de 30 geometrías
# Muchas goemetría no sirve solo con dos variables o con las características de esas variable
# Hay 74 extensiones que pueden instalarse: https://exts.ggplot2.tidyverse.org/gallery/ 

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_abline()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_area()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_bin_2d()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_bin2d()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_blank()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_boxplot()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_count()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_line()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_path()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_polygon()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_smooth()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_step()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_tile()

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_violin()

# Facets ----
# Agrupaciones, ejemplo un gráfico para hombres y otro para mujeres
# facet_wrap(~variable)
# En rectángulo, alargados, cuadrados, etc... 

# Statistics ----
# Si datos brutos no se ven bien por si solos, entonces se puede graficar la media, mediana, etc.
# Un summary()
# smooth = "lm" --> Hace una regresión
# smooth = "loess" --> Los errores
# stat_summary

# Coordinates ----
# Hacen alusión a los ejes --> Continuos, discretos, logarítmica, cuadrada, etc...
# Ejemplo: expand_limits(x = c(0,30), y = c(0, 150)) --> X de 0 a 30, Y de 0 a 150
# Ejemplo: ylim(0, 10)  --> Del eje Y
# Saltos de 10 en 10 los ejes, etc.

# Theme ----
# Son los fondos, hay muchos
# Colores, existen muchas paletas, hay paquetes de paletas de colores
# Escalas de colores continuas conocidas: viridis, magma, plasma, inferno, heat, etc...

# Cada capa se une con una suma "+"

#De wide format a long format, y al revés -----
# De una base de datos normal (cada fila una observación y cada columna una variable) --> wide format
# iris está en wide format

data <- as.numeric(iris)

data <- data %>%
  mutate(ID = c(1:150)) # Esta es la que se va a repetir para abajo

data$ID <- as.factor(data$ID) # La convertí a factor

data <- data %>%
  pivot_longer(c(1:4), names_to = "caracteristicas", values_to = "valor")

# En chupito de R dan otras dos opciones, en vídeos 2 de ggplot2!

data <- data %>%
  pivot_wider(id_cols = ID, names_from = caracteristicas, values_from = valor)

# Como hacer histogramas -----

set.seed(1234) #set de semillas 1234

df <- data.frame(
  sex = factor(rep(c("M", "H"), each = 200)),
  weight = round(c(rnorm(200, mean = 55, sd = 5), rnorm(200, mean = 65, sd = 5))))

# Histograma normal
ggplot(df, aes(x = weight)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "blue") +
  geom_vline(aes(xintercept = mean(weight)), color = "red", linetype = "dashed", size = 1.5)

ggplot(df, aes(x = weight, fill = sex, color = sex))+
  geom_histogram(aes(y = ..density..), alpha = .6, position = "identity") + #identify hacen que se superpongan las barras
  geom_density(alpha = .3)

?geom_histogram

#install.packages("plyr")
library(plyr)
mu <- ddply(df, "sex", summarise, grp.mean = mean(weight))

ggplot(df, aes(x = weight, fill = sex, color = sex))+
  geom_histogram(aes(y = ..density..), alpha = .6, position = "identity") + #identify hacen que se superpongan las barras
  geom_density(alpha = .3) +
  geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed", size = 1.5) +
  theme_minimal()  #le cambié fondito

ggplot(df, aes(x = weight, fill = sex, color = sex)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +  #bindwidth son cuantas barras quiero
  geom_vline(data = mu, aes(xintercept = grp.mean, color = sex), linetype = "dashed", size = 1.5) +
  scale_color_brewer(palette = "Dark2") +   #cambiamos colores a una paleta existente, puede ser manual también
  scale_fill_brewer(palette = "Dark2") 



