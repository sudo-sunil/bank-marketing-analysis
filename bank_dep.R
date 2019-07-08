bank <- read.csv("bank.csv",header = TRUE, stringsAsFactors = TRUE ,sep = ";")
View(bank)
summary(bank)
bank[,c(2:5,7:9,11,16,17)]<-sapply(bank[,c(2:5,7:9,11,16,17)],as.numeric)

cor(bank$y,bank$age) #0.0450918 
cor(bank$y,bank$job)  #0.02740081 
cor(bank$y,bank$marital) #0.01504188
cor(bank$y,bank$education) # 0.04298682
cor(bank$y,bank$default)  #0.001302653
cor(bank$y,bank$balance) #0.0179051
cor(bank$y,bank$housing) #-0.1046834 yesssss
cor(bank$y,bank$loan)   #-0.07051704  yess
cor(bank$y,bank$contact) #-0.1335952 yessss
cor(bank$y,bank$day) #-0.01124421
cor(bank$y,bank$month) #-0.04093276
#not checking for duration because it might not give a real analysis
cor(bank$y,bank$campaign) #-0.06114743
cor(bank$y,bank$pdays) #0.1040868 yessss
cor(bank$y,bank$previous) #0.1167144 yesss
cor(bank$y,bank$poutcome) #-0.08263197 yess

#poutcome,previous,pdays,campaign,contact,loan,housing,education, age
#checking for importance
model1 <- lm(y~.,data = bank)
model1
summary(model1)
model2 <- lm(y~(poutcome,previous,pdays,campaign,contact,loan,housing,education, age),data=bank)
model2
summary(model2)
#cleaning
library(magrittr) #for pipe
library(dplyr) 
cl_bank <- select(bank, c(age, education, loan, housing, contact, duration, pdays, previous,poutcome,y))
head(cl_bank)
#training
set.seed(3033)
intrain <- createDataPartition(y = cl_bank$y, p= 0.8, list = FALSE)
data_train <- cl_bank[intrain,]
data_test <- cl_bank[-intrain,]

dim(data_train)
dim(data_test)
dim(cl_bank)
View(cl_bank)
summary(intrain)

#-----------------------building model-----------------------------------------
library(rpart.plot)
library(rpart)
fit <- rpart(y~., data = data_train, method = 'class')
rpart.plot(fit)
dev.off()
fit
View(data_train)

#------------------predict unseen sample-----------------------------------------
#confusion matrix : how much the data is confusing the prediction
predict_unseen <- predict(fit, data_test, type = 'class')
table_mat <- table(data_test$y, predict_unseen)
table_mat

#-----------------measure performance of confusion matrix------------------------
accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_test #0.9081858

accuracy_tune <- function(fit){
  predict_unseen <- predict(fit,data_test,type='class')
  table_mat<-table(data_test$y, predict_unseen)
  accuracy_test1 <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_test1
}
#this is MACHINE LEARNING rest all is DATA MINING
control <- rpart.control(minsplit = 4, minbucket = round(5/3), maxdepth = 3, cp=0)
tune_fit <- rpart(y~., data = data_train, method='class', control = control)

accuracy_tune(tune_fit) #0.8904867
