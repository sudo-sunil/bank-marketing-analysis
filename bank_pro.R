getwd()
setwd("C:/Users/gbn_1/Documents/ML_R/Project-Bank")
bank <- read.csv("bank.csv",header = TRUE, stringsAsFactors = TRUE ,sep = ";")
str(bank)
bank
View(bank)
summary(bank)
head(bank)
hist(bank$y)
hist <- as.numeric(bank$y)
table(bank$balance)
plot(x = bank$y , y = bank$loan, xlab ="loan"  , ylab = "deposit", main ="hkj" )

library(magrittr) #for pipe
library(dplyr) 
cl_bank <- select(bank, c(age, education, balance, loan, job,y))
head(cl_bank)
#factorize data
#cl_bank$education <- factor(cl_bank$education, levels = c(1,2,3,4),labels = c('primary','secondary','tertiary','unknown'))
str(cl_bank)
glimpse(cl_bank)
View(cl_bank)

cl_bank[,c(2,4:6)]<-sapply(cl_bank[,c(2,4:6)],as.numeric)
View(cl_bank)

cor(cl_bank$age,cl_bank$y)
cor(cl_bank$education,cl_bank$age)
cor(cl_bank$loan,cl_bank$y)
cor(cl_bank$balance,cl_bank$y)
cor(cl_bank$job,cl_bank$y)

as.integer(bank$y)
bank_y<- factor(as.numeric(cl_bank$y))
summary(cl_bank)
View(bank_y)



set.seed(3033)
intrain <- createDataPartition(y = cl_bank$y, p= 0.8, list = FALSE)
data_train <- cl_bank[intrain,]
data_test <- cl_bank[-intrain,]

dim(data_train)
dim(data_test)
dim(cl_bank)
View(cl_bank)
cor(bank$y,bank$balance)

#to check randomisation is correct i.e. enough yes
prop.table(table(data_train$y))
prop.table(table(data_test$y))

#-----------------------building model-----------------------------------------
library(rpart.plot)
library(rpart)
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit)
dev.off()
fit
View(data_train)

#------------------predict unseen sample-----------------------------------------
#confusion matrix : how much the data is confusing the prediction
predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$survived, predict_unseen)
table_mat

#-----------------measure performance of confusion matrix------------------------
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_test 
#accuracy should be between 70 to 95
# greater than 95 indicates that it is biased to only this data and cannot classify unseen data properly
#takes a parameter -> decision tree model

#----------------------tuning accuracy----------------------------------------
#not needed when accuracy is high
accuracy_tune <- function(fit){
  predict_unseen <- predict(fit,data_test,type='class')
  table_mat<-table(data_test$survived, predict_unseen)
  accuracy_test1 <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_test1
}
#this is MACHINE LEARNING rest all is DATA MINING
control <- rpart.control(minsplit = 4, minbucket = round(5/3), maxdepth = 3, cp=0)
tune_fit <- rpart(survived~., data = data_train, method='class', control = control)

accuracy_tune(tune_fit)

model1 <- lm(y~.,data = bank)
model1
summary(model1)
