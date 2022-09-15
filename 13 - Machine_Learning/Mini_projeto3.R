# -------------------------------------------------------------------------
#                             Mini Projeto 3
#     Prevendo a Inadimplência de Clientes com Machine Learning e Power BI
#
# -------------------------------------------------------------------------

setwd('//VBOXSVR/powerBi_study/13 - Machine_Learning')
getwd()

# Installing the packages for project
install.packages('Amelia')       # -> functions for work with missing values
install.packages('caret')        # -> Work with machine learning
install.packages('ggplot2')      # -> library for visualization
install.packages('dplyr')        # -> Work with data processing
install.packages('reshape')      # -> For data reshape 
install.packages('randomForest') # -> Machine Learning Model
install.packages('e1071')        # -> Machine Learning Model

library(Amelia)
library(ggplot2)
library(caret)
library(reshape)
library(randomForest)
library(dplyr)
library(e1071)


data = read.csv('data/dataset.csv')

dim(data)
str(data)
summary(data)

## ----------------------------- Exploratory analysis -------------------------

# Removing the first columns - ID
data['ID'] = NULL
dim(data)
View(data)

# Rename columns - Class
colnames(data)
colnames(data)[24] = 'inadimplente'
colnames(data)
View(data)

# Verifying missing values and removing of the dataset 
sapply(data, function(x) sum(is.na(x)))

?missmap
missmap(data, main = 'Missing values found')
data = na.omit(data)

# Renaming categories columns
colnames(data)[2] = 'Genero'
colnames(data)[3] = 'Escolaridade'
colnames(data)[4] = 'Estado_Civil'
colnames(data)[5] = 'Idade'
colnames(data)


# ----------- Convert int for categorical -----------

# Genero
View(data['Genero'])
str(data['Genero'])
summary(data['Genero'])
?cut
data$Genero = cut(
  data$Genero, 
  c(0, 1, 2), 
  labels = c('Masculino', 'Feminino')
)
View(data['Genero'])
str(data['Genero'])
summary(data['Genero'])

# Escolaridade
data$Escolaridade = cut(
  data$Escolaridade,
  c(0, 1, 2, 3, 4),
  labels = c('Pos Graduado',
             'Graduado',
             'Ensino Medio',
             'Outros'
  )
)
View(data['Escolaridade'])
str(data['Escolaridade'])
summary(data['Escolaridade'])

# Estado Civil 
data$Estado_Civil = cut(
  data$Estado_Civil,
  c(-1, 0, 1, 2, 3),
  labels = c('Desconhecido',
             'Casado',
             'Solteiro',
             'Outro'
  )
)
View(data['Estado_Civil'])
str(data['Estado_Civil'])
summary(data['Estado_Civil'])

# Faixa Etária
data$Idade = cut(
  data$Idade,
  c(0, 30, 50, 100),
  labels = c('Jovem',
             'Adulto',
             'Idoso'
  )
)
View(data['Idade'])
str(data['Idade'])
summary(data['Idade'])


# as.factor - change only the variable type
data$PAY_0 = as.factor(data$PAY_0)
data$PAY_2 = as.factor(data$PAY_2)
data$PAY_3 = as.factor(data$PAY_3)
data$PAY_4 = as.factor(data$PAY_4)
data$PAY_5 = as.factor(data$PAY_5)
data$PAY_6 = as.factor(data$PAY_6)

# dataset after changes
str(data)
sapply(data, function(x) sum(is.na(x)))
missmap(data, main = 'Missing values found')
data = na.omit(data)
missmap(data, main = 'Missing values found')
dim(data)
View(data)

# Convert dependent variable for factor
data$inadimplente = as.factor(data$inadimplente)
str(data['inadimplente'])
View(data['inadimplente'])

table(data$inadimplente)
prop.table(table(data$inadimplente))

  ## plot 
qplot(inadimplente, data = data, geom = 'bar') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Set seed 
set.seed(12345)

# Stratified Sample
?createDataPartition
index = createDataPartition(data$inadimplente, p = 0.75, list = FALSE)
dim(index)

# Defining train data
train_data = data[index, ]
table(train_data$inadimplente)

prop.table(table(train_data$inadimplente))

# comparing percentage between train data and original data
compary_data = cbind(
  prop.table(table(data$inadimplente)),
  prop.table(table(train_data$inadimplente))
)
colnames(compary_data) = c('Treinamento', 'Original')
compary_data

# Melt data - Convert columns in rows
?reshape::melt
melt_compary_data = melt(compary_data)
melt_compary_data

# Plot - Train x Original
ggplot(
  melt_compary_data, 
  aes(x = X1, y = value)) +
geom_bar(aes(fill = X2), stat = 'identity', position = 'dodge') + 
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Test data
test_data = data[-index, ]
dim(test_data)


# ------------------------------- Models ----------------------------------

# ------------------------- 1 version
?randomForest
model_v1 = randomForest(inadimplente ~ ., data = train_data)
model_v1


# Evaluating model
plot(model_v1)

# prediction with train data
predicted_v1 = predict(model_v1, test_data)

# Confusion matrix 
?caret::confusionMatrix
cm_v1 = caret::confusionMatrix(predicted_v1, test_data$inadimplente, positive = "1")
cm_v1

# Calculating Precision, Recall and F1-Score, metrics of evaluation 
y = test_data$inadimplente
pred_y_1 = predicted_v1

precision = posPredValue(pred_y_1, y)
precision

recall = sensitivity(pred_y_1, y)
recall

f1 = (2 * precision * recall) / (precision + recall)
f1

# ------------------------- 2 version
# Balancing of class
install.packages('performanceEstimation')
library(performanceEstimation)
?smote

table(train_data$inadimplente)
prop.table(table(train_data$inadimplente))
set.seed(9560)
train_data_bal = smote(inadimplente ~ ., data = train_data)
table(train_data_bal$inadimplente)
prop.table(table(train_data_bal$inadimplente))

# building model 
model_v2 = randomForest(inadimplente ~ .,data = train_data_bal)
model_v2

# Evaluating model
plot(model_v2)

# prediction with train data
predicted_v2 = predict(model_v2, test_data)

# Confusion matrix 
?caret::confusionMatrix
cm_v2 = caret::confusionMatrix(predicted_v2, test_data$inadimplente, positive = "1")
cm_v2

# Calculating Precision, Recall and F1-Score, metrics of evaluation 
y = test_data$inadimplente
pred_y_2 = predicted_v2

precision = posPredValue(pred_y_2, y)
precision

recall = sensitivity(pred_y_2, y)
recall

f1 = (2 * precision * recall) / (precision + recall)
f1

# most relevant variables
varImpPlot(model_v2)

imp_var = importance(model_v2)
importance_var = data.frame(Variables = row.names(imp_var),
                           Importance = round(imp_var[, 'MeanDecreaseGini'], 2))

# Creating variables rank per importance
importance_rank = importance_var %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Visualizing
ggplot(
  importance_rank, 
  aes(x = reorder(Variables, Importance),
      y = Importance,
      fill = Importance)
  ) +
  geom_bar(stat='identity') +
  geom_text(
    aes(x = Variables, y = 0.5, label = Rank),
    hjust = 0,
    vjust = 0.55,
    size = 4,
    colour = 'red'
  ) +
  labs(x = 'Variables') +
  coord_flip()


# ------------------------- 3 version
colnames(train_data_bal)
model_v3 = randomForest(
  inadimplente ~ PAY_0 + LIMIT_BAL + BILL_AMT1 + PAY_AMT2 + PAY_AMT1 + PAY_AMT6 + BILL_AMT2,
  data = train_data_bal
)
model_v3

# Evaluating model
plot(model_v3)

# prediction with train data
predicted_v3 = predict(model_v3, test_data)

# Confusion matrix 
?caret::confusionMatrix
cm_v3 = caret::confusionMatrix(predicted_v3, test_data$inadimplente, positive = "1")
cm_v3

# Calculating Precision, Recall and F1-Score, metrics of evaluation 
y = test_data$inadimplente
pred_y_3 = predicted_v3

precision = posPredValue(pred_y_3, y)
precision

recall = sensitivity(pred_y_3, y)
recall

f1 = (2 * precision * recall) / (precision + recall)
f1

# Saving model 
saveRDS(model_v3, file = 'model/model_v3.rds')

# loading model
final_model = readRDS('model/model_v3.rds')

# Predict new costumers
  ## costumers data
PAY_0 = c(0, 0, 0) 
LIMIT_BAL = c(45000, 50000, 35000)
BILL_AMT1 = c(300, 400, 280)
PAY_AMT2 = c(1500, 1300, 1150)
PAY_AMT1 = c(1100, 1000, 1200)
PAY_AMT6 = c(1000, 1400, 1350)
BILL_AMT2 = c(350, 420, 280)

new_costumers = data.frame(PAY_0, LIMIT_BAL, BILL_AMT1, PAY_AMT2, PAY_AMT1, PAY_AMT6, BILL_AMT2)
View(new_costumers)

# Predictions
costumers_predictions = predict(final_model, new_costumers) # Error

# before
str(train_data_bal)
str(new_costumers)

  ## Convert new data
new_costumers$PAY_0 <- factor(new_costumers$PAY_0, levels = levels(train_data_bal$PAY_0))
str(new_costumers)

  ## Prediction right
costumers_predictions = predict(final_model, new_costumers)
View(costumers_predictions)