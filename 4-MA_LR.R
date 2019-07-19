# Start separate data cleaning script
setwd("C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/Code")
source("1-MA_Data Cleaning_NEW.R", echo=TRUE)



#################################################
######## DATA PARTITIONING ######################
#################################################

##split data into 90% training and 10% test
set.seed(1)
train_index <- createDataPartition(telco$churn, p = .9, list = FALSE, times = 1)
train <- telco[train_index,]
test <- telco[-train_index,]




############################################################################################
######## Predictive LOGISTIC REGRESSION ####################################################
############################################################################################


### MODEL WITH ALL VARIABLES ###------------------------------------------------------------

## Train
trainctrl <- trainControl(method="none", verboseIter=TRUE)
time.all <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  LRmodel.all <- train(churn ~ ., data=train, 
                       method="glm", family="binomial", 
                       trControl = trainctrl) 
)))["elapsed",]
LRmodel.all
summary(LRmodel.all)

## Evaluate performance on training data
train.eval <- train
train.eval$pred <- predict(LRmodel.all, newdata = train.eval)
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## Evaluate performance on test data
test$pred <- predict(LRmodel.all, newdata = test)
confusionMatrix(data=test$pred, reference=test$churn, positive="1")


# Variable importance
LRImp <- varImp(LRmodel.all, scale=FALSE, useModel = FALSE)
plot(LRImp, top = 20) # plot all variables' importance




### MODEL WITH TOP 10 VARIABLES ###------------------------------------------------------------

# extract top 10 variables
LRImp_df <- as.data.frame(LRImp$importance)
LRImp_df$X1 <- NULL
colnames(LRImp_df)[colnames(LRImp_df)=="X0"] <- "Imp"
library(tibble)
LRImp_df <- LRImp_df %>% rownames_to_column("Var") #copy rownames to own column
LRImp_df <- top_n(LRImp_df, 10, Imp) #select top 10 variables by importance
selectVar <- as.vector(LRImp_df$Var) #create vector with all top 10 variables
selectVar <- append(selectVar, "churn") #add churn to selected variables vector

#create subsets with top10 vars & churn
train.top10 <- train[,selectVar]
test.top10 <- test[,selectVar]

## train top 10 model
trainctrl <- trainControl(method="none", verboseIter=TRUE)
time.top10 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  LRmodel.top10 <- train(churn ~ ., data=train.top10, 
                         method="glm", family="binomial", 
                         trControl = trainctrl) 
)))["elapsed",]
LRmodel.top10
summary(LRmodel.top10)

## Evaluate performance on training data
train.eval <- train.top10
train.eval$pred <- predict(LRmodel.top10, newdata = train.eval)
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## Evaluate performance on test data
test.top10$pred <- predict(LRmodel.top10, newdata = test.top10)
confusionMatrix(data=test.top10$pred, reference=test.top10$churn, positive="1")




### MODEL WITH TOP 25 VARIABLES ###------------------------------------------------------------

# extract top 25 variables
LRImp_df <- as.data.frame(LRImp$importance)
LRImp_df$X1 <- NULL
colnames(LRImp_df)[colnames(LRImp_df)=="X0"] <- "Imp"
library(tibble)
LRImp_df <- LRImp_df %>% rownames_to_column("Var") #copy rownames to own column
LRImp_df <- top_n(LRImp_df, 25, Imp) #select top 25 variables by importance
selectVar <- as.vector(LRImp_df$Var) #create vector with all top 25 variables
selectVar <- append(selectVar, "churn") #add churn to selected variables vector

#create subsets with top25 vars & churn
train.top25 <- train[,selectVar]
test.top25 <- test[,selectVar]

## train top 25 model
trainctrl <- trainControl(method="none", verboseIter=TRUE)
time.top25 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  LRmodel.top25 <- train(churn ~ ., data=train.top25, 
                         method="glm", family="binomial", 
                         trControl = trainctrl) 
)))["elapsed",]
LRmodel.top25
summary(LRmodel.top25)

## Evaluate performance on training data
train.eval <- train.top25
train.eval$pred <- predict(LRmodel.top25, newdata = train.eval)
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## Evaluate performance on test data
test.top25$pred <- predict(LRmodel.top25, newdata = test.top25)
confusionMatrix(data=test.top25$pred, reference=test.top25$churn, positive="1")




### MODEL WITH TOP 50 VARIABLES ###------------------------------------------------------------

# extract top 50 variables
LRImp_df <- as.data.frame(LRImp$importance)
LRImp_df$X1 <- NULL
colnames(LRImp_df)[colnames(LRImp_df)=="X0"] <- "Imp"
library(tibble)
LRImp_df <- LRImp_df %>% rownames_to_column("Var") #copy rownames to own column
LRImp_df <- top_n(LRImp_df, 50, Imp) #select top 50 variables by importance
selectVar <- as.vector(LRImp_df$Var) #create vector with all top 50 variables
selectVar <- append(selectVar, "churn") #add churn to selected variables vector

#create subsets with top50 vars & churn
train.top50 <- train[,selectVar]
test.top50 <- test[,selectVar]

## train top 50 model
trainctrl <- trainControl(method="none", verboseIter=TRUE)
time.top50 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  LRmodel.top50 <- train(churn ~ ., data=train.top50, 
                         method="glm", family="binomial", 
                         trControl = trainctrl) 
)))["elapsed",]
LRmodel.top50
summary(LRmodel.top50)

## Evaluate performance on training data
train.eval <- train.top50
train.eval$pred <- predict(LRmodel.top50, newdata = train.eval)
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## Evaluate performance on test data
test.top50$pred <- predict(LRmodel.top50, newdata = test.top50)
confusionMatrix(data=test.top50$pred, reference=test.top50$churn, positive="1")




### MODEL WITH TOP 75 VARIABLES ###------------------------------------------------------------

# extract top 75 variables
LRImp_df <- as.data.frame(LRImp$importance)
LRImp_df$X1 <- NULL
colnames(LRImp_df)[colnames(LRImp_df)=="X0"] <- "Imp"
library(tibble)
LRImp_df <- LRImp_df %>% rownames_to_column("Var") #copy rownames to own column
LRImp_df <- top_n(LRImp_df, 75, Imp) #select top 75 variables by importance
selectVar <- as.vector(LRImp_df$Var) #create vector with all top 75 variables
selectVar <- append(selectVar, "churn") #add churn to selected variables vector

#create subsets with top75 vars & churn
train.top75 <- train[,selectVar]
test.top75 <- test[,selectVar]

## train top 75 model
trainctrl <- trainControl(method="none", verboseIter=TRUE)
time.top75 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  LRmodel.top75 <- train(churn ~ ., data=train.top75, 
                         method="glm", family="binomial", 
                         trControl = trainctrl) 
)))["elapsed",]
LRmodel.top75
summary(LRmodel.top75)

## Evaluate performance on training data
train.eval <- train.top75
train.eval$pred <- predict(LRmodel.top75, newdata = train.eval)
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## Evaluate performance on test data
test.top75$pred <- predict(LRmodel.top75, newdata = test.top75)
confusionMatrix(data=test.top75$pred, reference=test.top75$churn, positive="1")
