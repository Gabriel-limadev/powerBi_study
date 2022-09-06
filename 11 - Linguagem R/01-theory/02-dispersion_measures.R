# Basic Statistic

# --------------------- 2 - Dispersion measures ---------------------

# Defining the folder
setwd("/home/gabriel-liamdev/Documents/GitHub/powerBi_study/11 - Linguagem R/01-theory")
getwd()

# Loading the dataset 
vendas = read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Dataset Resume
View(vendas)
str(vendas)
summary(vendas$Valor)

# variance
var(vendas$Valor)

# standard deviation
sd(vendas$Valor)
