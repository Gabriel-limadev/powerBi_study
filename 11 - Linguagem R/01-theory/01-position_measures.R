# Basic Statistic

# --------------------- 1 - Position measures ---------------------

# Defining the folder
setwd("/home/gabriel-liamdev/Documents/GitHub/powerBi_study/11 - Linguagem R/01-theory")
getwd()

# Loading the dataset 
vendas = read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Dataset Resume
View(vendas)
str(vendas)
summary(vendas$Valor)
summary(vendas$Custo)

# Average 
?mean
mean(vendas$Valor)
mean(vendas$Custo)

# Weighted average
?weighted.mean
weighted.mean(vendas$Valor, w = vendas$Custo)

# Median 
median(vendas$Valor)
median(vendas$Custo)

# Mode

mode = function(v){
  unique_value = unique(v)
  unique_value[which.max(tabulate(match(v, unique_value)))]
}

# getting mode
valor_mode = mode(vendas$Valor)
custo_mode = mode(vendas$Custo)
print(valor_mode)
print(custo_mode)



# ---------------------- 1.1 Creating Graphics --------------------------

install.packages("ggplot2")
library(ggplot2)

ggplot(vendas) + 
  stat_summary(aes(x = Estado,
                   y = Valor),
               fun = mean,
               geom = "bar",
               fill = "lightgreen",
               col = "grey50") +
  labs(title = "Média de Valor por Estado")