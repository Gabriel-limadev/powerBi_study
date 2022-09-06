# Basic Graphics with R

# http://www.r-graph-gallery.com/

setwd('/home/gabriel-liamdev/Documents/GitHub/powerBi_study/11 - Linguagem R/03-graphics')
getwd()

# data
result_total_vector = c(3, 12, 5, 18, 45) 
names(result_total_vector) = c('A', 'B', 'C', 'D', 'E')
result_total_vector

#### Barplot ####
?barplot
barplot(result_total_vector)
barplot(result_total_vector, col = c(1, 2, 3, 4, 5))

# Salving the graphic in disk
png("barplot.png", width = 480, height = 480 )
barplot(result_total_vector, 
        col = rgb(0.5,0.1,0.6,0.6), 
        xlab = "Categorias", 
        ylab = "Valores", 
        main = "Barplot em R", 
        ylim = c(0,60) )
dev.off()

# Ggplot2 
library(ggplot2)
View(mtcars)

# Barplot
ggplot(mtcars, aes(x = as.factor(cyl) )) + 
  geom_bar()

ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue"))

# Create dummy data (fictícios)
dados = data.frame(group = c("A ","B ","C ","D ") , value = c(33,62,56,67))
View(dados)

# Barplot
ggplot(dados, aes(x = group, y = value ,fill = group )) + 
  geom_bar(width = 0.85, stat = "identity")


##### Pie Chart ##### 

fatias <- c(4, 12, 14, 16, 8)
paises <- c("Brasil", "Estados Unidos", "Alemanha", "Reino Unido", "Espanha")
?pie
pie(fatias, labels = paises, main = "Leitura de Livros Por Pessoa/Ano")

# Pie Chart 3D
install.packages("plotrix")
library(plotrix)

fatias <- c(4, 12, 14, 16, 8)
paises <- c("Brasil", "Estados Unidos", "Alemanha", "Reino Unido", "Espanha")
?pie3D
pie3D(fatias, labels = paises, explode = 0.1, main = "Leitura de Livros Por Pessoa/Ano")


##### Line Chart ##### 

# Data
cars <- c(1, 3, 6, 4, 9)
paths <- c(2, 5, 4, 5, 12)

# Plot
plot(cars, type = "o", col = "blue", ylim = c(0,12))

lines(paths, type = "o", pch = 22, lty = 2, col = "red")

title(main = "Produção de Veículos", col.main = "red", font.main = 4)


##### Boxplot ##### 

library(ggplot2)
View(mpg)

# Plot
ggplot(mpg, aes(x = reorder(class, hwy), y = hwy, fill = class)) + 
  geom_boxplot() +
  xlab("class") +
  theme(legend.position = "none")


##### Scatter Plot ##### 

library(ggplot2)
data = data.frame(cond = rep(c("condition_1", "condition_2"), each=10), 
                  my_x = 1:100 + rnorm(100,sd=9), my_y = 1:100 + rnorm(100,sd=16))

View(data)

ggplot(data, aes(x=my_x, y=my_y)) + 
  geom_point(shape=1)

# Add regression line
ggplot(data, aes(x=my_x, y=my_y)) +    
  geom_point(shape=1) +  
  geom_smooth(method = lm , color="red", se=FALSE) 

# add smooth
ggplot(data, aes(x=my_x, y=my_y)) +    
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="red", se=TRUE)  


##### Treemap #####

install.packages("treemap")
library(treemap)

# Data
grupo = c(rep("grupo-1",4), rep("grupo-2",2), rep("grupo-3",3))
subgrupo = paste("subgroup", c(1,2,3,4,1,2,1,2,3), sep = "-")
valor = c(13,5,22,12,11,7,3,1,23)
dados = data.frame(grupo, subgrupo, valor)
View(dados)

# Labels
?treemap
treemap(dados, 
        index = c("grupo", "subgrupo"),     
        vSize = "valor", 
        type = "index",
        fontsize.labels = c(15,12),               
        fontcolor.labels = c("white", "orange"),    
        fontface.labels = c(2,1),                 
        bg.labels = 220,             
        align.labels = list(c("center", "center"), c("right", "bottom")),                                  
        overlap.labels = 0.5,           
        inflate.labels = F)


# Custumizing
treemap(dados, 
        index = c("grupo", "subgrupo"), 
        vSize = "valor", 
        type = "index",
        border.col = c("black", "white"),          
        border.lwds = c(7,2))


##### Histograma #####

# Gerando valores para x
x <- mtcars$mpg 

# Criando o histograma
?hist
h <- hist(x, 
          breaks = 10, 
          col = "red", 
          xlab = "Milhas Por Galão", 
          main = "Histograma com Curva de Distribuição") 

# Customizando o histograma
xfit <- seq(min(x), max(x), length = 40) 
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col = "blue", lwd = 2)

# Usando o ggplot2
library(ggplot2)

# dataset
dados = data.frame(value = rnorm(10000))
View(dados)

# Tamanho das colunas
ggplot(dados, aes(x=value)) + 
  geom_histogram(binwidth = 0.05)

# Cor uniforme
ggplot(dados, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, color="white", fill=rgb(0.2,0.7,0.1,0.4) ) 

# Cor proporcional
ggplot(dados, aes(x=value)) + 
  geom_histogram(binwidth = 0.2, aes(fill = ..count..) )
