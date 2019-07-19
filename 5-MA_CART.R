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
######## CLASSIFICATION TREE ###############################################################
############################################################################################

## based on conditional inference tree approach

##Estimate Classifier 
library(party)
time.CT <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  CTmodel1 <- ctree(churn ~ ., data = train, controls = ctree_control(maxdepth = 5))
)))["elapsed",]

# Plot Classification Tree
plot(CTmodel1, type="simple")

## evaluate performance on training data
train.eval <- train
train.eval$pred <- predict(CTmodel1, newdata = train.eval)
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## evaluate performance on test data
test$pred <- predict(CTmodel1, newdata = test)
confusionMatrix(data=test$pred, reference=test$churn, positive="1")

# AUC
library(AUC)
r <- roc(test$pred, as.factor(test$churn))
auc(r)
plot(r)




