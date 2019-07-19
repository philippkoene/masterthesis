options(scipen=999)


############################################################################################
######## ANALYZE RESULTS from Random Grid Search ###########################################
############################################################################################


### IMPORT Results

ANNresults.RGS <- read.csv(file = "C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/ANN Results/tfruns on Jan machine/_results_RandomGridSearch_JansMachine_20190531.csv", 
                       header = TRUE) #Import old results csv
#add computation time
ANNresults.RGS$comp.time.s <- as.integer(difftime(ANNresults.RGS$end, ANNresults.RGS$start, units = "secs"))
ANNresults.RGS$comp.time.m <- as.double(difftime(ANNresults.RGS$end, ANNresults.RGS$start, units = "mins"))


### ANALYSIS

library("ggplot2")

#droprate
ANNresults.RGS$flag_droprate <- as.factor(ANNresults.RGS$flag_droprate)
p1 <- ggplot(ANNresults.RGS, aes(x=flag_droprate, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_droprate, y=metric_val_acc)) +
  labs(x = "Dropout Rate", y = NULL)


#nnodes
ANNresults.RGS$flag_nnodes <- as.factor(ANNresults.RGS$flag_nnodes)
p2 <- ggplot(ANNresults.RGS, aes(x=flag_nnodes, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_nnodes, y=metric_val_acc)) +
  labs(x = "Nr. of Nodes per Hidden Layer", y = NULL)

#lr
ANNresults.RGS$flag_lr <- as.factor(ANNresults.RGS$flag_lr)
p3 <- ggplot(ANNresults.RGS, aes(x=flag_lr, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_lr, y=metric_val_acc)) +
  labs(x = "Learning Rate", y = NULL)

#batch size
ANNresults.RGS$flag_batch_size <- as.factor(ANNresults.RGS$flag_batch_size)
p4 <- ggplot(ANNresults.RGS, aes(x=flag_batch_size, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_batch_size, y=metric_val_acc)) +
  labs(x = "Batch Size", y = NULL)

#merge plots in 2x2 grid
library("gridExtra")
grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 1,
  top = "Random Grid Search Results: Accuracy on Validation Set"
)


#difference in computation time by lr
aggregate(comp.time.m ~ flag_lr, ANNresults.RGS, mean)

summary(ANNresults.RGS$comp.time.m)
summary(ANNresults.RGS$comp.time.s)



############################################################################################
######## ANALYZE RESULTS from Pure Grid Search #############################################
############################################################################################

### IMPORT Results

setwd("C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/ANN Results/tfruns on Jan machine")

# Create data frame with all run results & parameters
library(tfruns)
results <- ls_runs()
results$model <- NULL
results$output <- NULL

#add computation time
results$comp.time.s <- round(as.double(difftime(results$end, results$start, units = "secs")), 2)
results$comp.time.m <- round(as.double(difftime(results$end, results$start, units = "mins")), 2)

#save results as csv -> specify the name of the csv file
write.csv(results, file = "_results.csv")#, row.names=FALSE)


### ANALYSIS


#regression plot
results$flag_nnodes <- as.factor(results$flag_nnodes)
ggplot(results, aes(x=flag_nHL, y=metric_val_acc, color=flag_nnodes)) +
  geom_point(shape = 16, size = 1, stroke = 1) +   
  scale_colour_hue(l=50) +
  geom_smooth(se=FALSE) + #method=lm, se=FALSE) +
  labs(x = "Nr. of Hidden Layers", y = NULL, colour = "Nodes \nper \nLayer") +
  ggtitle("Pure Grid Search Results: Accuracy on Validation Set") +
  theme(plot.title = element_text(hjust = 0.5))

#plot interaction of nHL & nnodes
results$flag_nHL <- as.factor(results$flag_nHL)
results$flag_nnodes <- as.factor(results$flag_nnodes)
ggplot(results, aes(x=flag_nHL, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_nHL, y=metric_val_acc, fill=flag_nnodes)) +
  labs(x = "Nr. of Hidden Layers", y = "Acc on Valid. Set", fill = "Nodes \nper \nLayer") #+
#ggtitle("Pure Grid Search Results:
#Interaction Effect between Nr. of Hidden Layers & Nr. of Nodes") +
#theme(plot.title = element_text(hjust = 0.5))



#droprate
results$flag_droprate <- as.factor(results$flag_droprate)
p5 <- ggplot(results, aes(x=flag_droprate, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_droprate, y=metric_val_acc)) +
  labs(x = "Dropout Rate", y = NULL)

#nnodes
results$flag_nnodes <- as.factor(results$flag_nnodes)
p6 <- ggplot(results, aes(x=flag_nnodes, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_nnodes, y=metric_val_acc)) +
  labs(x = "Nr. of Nodes per Hidden Layer", y = NULL)

#batch size
results$flag_batch_size <- as.factor(results$flag_batch_size)
p7 <- ggplot(results, aes(x=flag_batch_size, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_batch_size, y=metric_val_acc)) +
  labs(x = "Batch Size", y = NULL)

#nHL
results$flag_nHL <- as.factor(results$flag_nHL)
p8 <- ggplot(results, aes(x=flag_nHL, y=metric_val_acc)) +
  geom_boxplot(aes(x=flag_nHL, y=metric_val_acc)) +
  labs(x = "Nr. of Hidden Layers", y = NULL)

#merge plots in 2x2 grid
library("gridExtra")
grid.arrange(p5, p6, p7, p8, nrow = 1,
  top = "Pure Grid Search Results: Accuracy on Validation Set"
)


# computation time
summary(results$comp.time.m)
summary(results$comp.time.s)



############################################################################################
######## Evaluate Model on Training & Test Set #############################################
############################################################################################


#load final ANN model
setwd("C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/ANN Results")
library("keras")
model <- load_model_hdf5("ANN_FinalModel.h5")


## evaluate performance on training data
train.eval <- train.dummy.X
train.eval$pred <- as.factor(round(predict(model, train.dummy.X)))
#train.dummy.X <- as.factor(train.dummy.X)
confusionMatrix(data=train.eval$pred, reference=as.factor(train.dummy.Y), positive="1")

## evaluate performance on test data
test.eval <- test.dummy.X
test.eval$pred <- as.factor(round(predict(model, test.dummy.X)))
#train.dummy.X <- as.factor(train.dummy.X)
confusionMatrix(data=test.eval$pred, reference=as.factor(test.dummy.Y), positive="1")








# Variable Importance
library("NeuralNetTools")
olden(weights)
weights <- get_weights(model)
count_params(model)


