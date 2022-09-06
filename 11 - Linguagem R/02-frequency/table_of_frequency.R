# Frequency 

setwd("/home/gabriel-liamdev/Documents/GitHub/powerBi_study/11 - Linguagem R/02-frequency")
getwd()

data = read.table("Usuarios.csv",
                  dec = ".",
                  sep = ",",
                  h = T,
                  fileEncoding = "windows-1252")

View(data)
names(data)
str(data)
summary(data$salario)
summary(data$grau_instrucao)
mean(data$salario)
mean(data$grau_instrucao) # error - return: NA

# table of absolute frequency
freq = table(data$grau_instrucao)
View(freq)

# table of relative frequency
freq_rel = prop.table(freq)
p_freq_rel = prop.table(freq) * 100
View(freq_rel)

# add total lines
freq = c(freq, sum(freq))
names(freq)[4] = "Total"
View(freq)

# Table finished
freq_rel = c(freq_rel, sum(freq_rel))
p_freq_rel = c(p_freq_rel, sum(p_freq_rel))

final_table = cbind(freq, 
                    freq_rel = round(freq_rel, digits = 2),
                    p_freq_rel  = round(p_freq_rel, digits = 2))
View(final_table)

