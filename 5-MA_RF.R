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
######## RANDOM FOREST #####################################################################
############################################################################################


## Random Forest with all variables (except crclscod --> too many levels for algorithm)
## based on Breiman's random forest algorithm

train <- train[,-4]
test <- test[,-4]




############################################################################################
######## Hyperparameter Tuning #############################################################
############################################################################################

library("randomForest")

# default mtry = floor(sqrt(ncol(x))) --> mtry = 9
time.RF <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            importance=TRUE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.1 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.1 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                          mtry=1,  
                          importance=FALSE,
                          do.trace=TRUE)
)))["elapsed",]

time.RF.2 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.2 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=2,  
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.3 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.3 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=3,  
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.4 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.4 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=4,  
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.5 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.5 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=5,  #1 yields lowest OBB error 
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.6 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.6 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=6,  #1 yields lowest OBB error 
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.7 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.7 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=7,  #1 yields lowest OBB error 
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.8 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.8 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=8,  #1 yields lowest OBB error 
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.10 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.10 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                            mtry=10,  #1 yields lowest OBB error 
                            importance=FALSE,
                            do.trace=TRUE)
)))["elapsed",]

time.RF.11 <- as.data.frame(as.matrix(system.time(  #store elapsed computation time
  RFmodel.11 <- randomForest(churn ~ ., data=train, ntree=100,  #no significant increases after the 100th tree
                             mtry=11,  #1 yields lowest OBB error 
                             importance=FALSE,
                             do.trace=TRUE)
)))["elapsed",]




############################################################################################
######## MODEL EVALUATION ##################################################################
############################################################################################


## evaluate performance on training data
train.eval <- train
train.eval$pred <- predict(RFmodel)  # !!! do not use argument "newdata = train.eval": https://stats.stackexchange.com/questions/111968/random-forest-how-to-handle-overfitting
confusionMatrix(data=train.eval$pred, reference=train.eval$churn, positive="1")

## evaluate performance on test data
test$pred <- predict(RFmodel, newdata = test)
confusionMatrix(data=test$pred, reference=test$churn, positive="1")


# Inspect model results
OBB <- as.data.frame(RFmodel$err.rate)
OBB$iter <- rownames(OBB)
OBB$"0" <- NULL
OBB$`1` <- NULL
OBB <- OBB[c(2,1)]
plot(OBB)

summary(RFmodel.8)
RFmodel$importance
RFmodel$err.rate
RFmodel$forest

## Variable Importance
RF.varimp <- as.data.frame(RFmodel$importance)
varImpPlot(RFmodel, scale=FALSE, n.var=10)




