# Basic Statistic

# --------------------- 3 - Relative Position measures ---------------------

# Defining the folder
setwd("/home/gabriel-liamdev/Documents/GitHub/powerBi_study/11 - Linguagem R/01-theory")
getwd()

# Loading the dataset 
vendas = read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Data Resume
head(vendas)
tail(vendas)
View(vendas)

# measures of central tendency
summary(vendas$Valor)
summary(vendas[c('Valor', 'Custo')])

# Exploring numeric variables
mean(vendas$Valor)
median(vendas$Valor)
quantile(vendas$Valor)
quantile(vendas$Valor, probs = c(0.01, 0.99))
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20))
IQR(vendas$Valor)
range(vendas$Valor)
summary(vendas$Valor)
diff(range(vendas$Valor))




