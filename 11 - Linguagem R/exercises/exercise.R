# Exercises

setwd("/home/gabriel-liamdev/Documents/GitHub/powerBi_study/11 - Linguagem R/exercises")
getwd()

grades = read.csv("Notas.csv", fileEncoding = "windows-1252")
View(grades)

# 1) Presents a summary of data types and dataset statistics
str(grades)
summary(grades$TurmaA)
summary(grades$TurmaB)

# 2) What is the average in each class?
mean(grades$TurmaA)
mean(grades$TurmaB)

# 3) What class had the greatest variability in grades? 
sd(grades$TurmaA)
sd(grades$TurmaB)
# Response: TurmaA because your standard deviation is more than TurmaB

# 4) Calculate the coefficient of variation of the 2 class:
average_turmaA = mean(grades$TurmaA)
average_turmaB = mean(grades$TurmaB)

sd_ta = sd(grades$TurmaA)
sd_tb = sd(grades$TurmaB)

cvA = sd_ta / average_turmaA * 100
cvB = sd_tb / average_turmaB * 100
print(cvA)
print(cvB)

# 5) What grade appeared more often in each class?
mode = function(v){
  unique_value = unique(v)
  unique_value[which.max(tabulate(match(v, unique_value)))]
}

# getting mode
turmaA_mode = mode(grades$TurmaA)
turmaB_mode = mode(grades$TurmaB)
print(turmaA_mode)
print(turmaB_mode)
