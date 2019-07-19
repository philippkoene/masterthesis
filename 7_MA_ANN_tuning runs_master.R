##############################################################################
######## DATA CLEANING: start separate script ################################
##############################################################################

setwd("C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/Code")
source("1-MA_Data Cleaning_NEW.R", echo=TRUE)




#################################################
######## ANN: NORMALIZATION & DUMMIES ###########
#################################################

### DATA NORMALIZATION/SCALING

# Scale all numerical values within a range of [0;1]
library("BBmisc")
telco.norm <- normalize(telco, method = "range", range = c(0,1))  #automatically only applies to numerical values

# Automatically apply One-Hot-Encoding (OHE) to all non-binary factors (with >2 levels)
levels <- as.data.frame(unlist(lapply(telco.norm, nlevels)))  #get list of all factors' levels
colnames(levels)[colnames(levels)=="unlist(lapply(telco.norm, nlevels))"] <- "n"  #rename column
library(tibble)
levels <- levels %>% rownames_to_column("Var")  #copy rownames to own column
levels <- levels[which(levels$"n" > 2), ]  #select all factors with >2 levels
selectVar3 <- as.vector(levels$Var)  #store them in a vector
library("dummies")
telco.norm <- dummy.data.frame(selectVar3, data=telco.norm)  #create dummy variables




#################################################
######## DATA PARTITIONING ######################
#################################################

##split data into 90% training and 10% test
set.seed(1)
train_index <- createDataPartition(telco.norm$churn, p = .9, list = FALSE, times = 1)
train <- telco.norm[train_index,]
test <- telco.norm[-train_index,]




##############################################################################
######## ANN DATASET PREPARATIONS ############################################
##############################################################################

library(keras)
library(tensorflow)

## Separate input variables (X) from output/churn (Y)

# Train
train.dummy.X <- train
train.dummy.X$churn <- NULL
train.dummy.X <- data.matrix(train.dummy.X) #convert to matrix, required by keras::fit

train.dummy.Y <- to_categorical(train$churn)

# Test
test.dummy.X <- test
test.dummy.X$churn <- NULL
test.dummy.X <- data.matrix(test.dummy.X) #convert to matrix, required by keras::fit

test.dummy.Y <- to_categorical(test$churn)


## Chop input data into ranges of columns matching the input layers
train.dummy.X <-
  list(
    train.dummy.X[, 1:6, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 7:12, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 13:32, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 33:86, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 87:89, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 90:105, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 106:108, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 109:126, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 127:129, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 130:136, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 137:146, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 147:149, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 150:152, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 153:155, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 156:158, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 159:161, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 162:164, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 165:169, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 170:175, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 176:178, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 179:182, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 183:185, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 186:191, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 192:194, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 195:197, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 198:202, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 203:203, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 204:207, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 208:208, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 209:213, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 214:217, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 218:222, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 223:223, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 224:224, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 225:229, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 230:230, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 231:231, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 232:232, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 233:233, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 234:234, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 235:235, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 236:236, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 237:237, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 238:238, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 239:239, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 240:240, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 241:241, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 242:242, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 243:243, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 244:244, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 245:245, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 246:246, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 247:247, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 248:248, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 249:249, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 250:250, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 251:251, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 252:252, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 253:253, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 254:254, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 255:255, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 256:256, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 257:257, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 258:258, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 259:259, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 260:260, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 261:261, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 262:262, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 263:263, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 264:264, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 265:265, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 266:266, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 267:267, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 268:268, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 269:269, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 270:270, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 271:271, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 272:272, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 273:273, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 274:274, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 275:275, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 276:276, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 277:277, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 278:278, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 279:279, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 280:280, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 281:281, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 282:282, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 283:283, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 284:284, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 285:285, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 286:286, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 287:287, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 288:288, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 289:289, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 290:290, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 291:291, drop = FALSE] %>% as.matrix(),
    train.dummy.X[, 292:292, drop = FALSE] %>% as.matrix()
  )

test.dummy.X <-
  list(
    test.dummy.X[, 1:6, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 7:12, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 13:32, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 33:86, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 87:89, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 90:105, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 106:108, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 109:126, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 127:129, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 130:136, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 137:146, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 147:149, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 150:152, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 153:155, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 156:158, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 159:161, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 162:164, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 165:169, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 170:175, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 176:178, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 179:182, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 183:185, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 186:191, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 192:194, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 195:197, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 198:202, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 203:203, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 204:207, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 208:208, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 209:213, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 214:217, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 218:222, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 223:223, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 224:224, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 225:229, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 230:230, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 231:231, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 232:232, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 233:233, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 234:234, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 235:235, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 236:236, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 237:237, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 238:238, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 239:239, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 240:240, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 241:241, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 242:242, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 243:243, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 244:244, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 245:245, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 246:246, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 247:247, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 248:248, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 249:249, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 250:250, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 251:251, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 252:252, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 253:253, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 254:254, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 255:255, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 256:256, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 257:257, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 258:258, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 259:259, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 260:260, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 261:261, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 262:262, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 263:263, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 264:264, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 265:265, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 266:266, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 267:267, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 268:268, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 269:269, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 270:270, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 271:271, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 272:272, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 273:273, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 274:274, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 275:275, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 276:276, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 277:277, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 278:278, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 279:279, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 280:280, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 281:281, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 282:282, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 283:283, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 284:284, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 285:285, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 286:286, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 287:287, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 288:288, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 289:289, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 290:290, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 291:291, drop = FALSE] %>% as.matrix(),
    test.dummy.X[, 292:292, drop = FALSE] %>% as.matrix()
  )




##############################################################################
######## Hyperparameter Tuning Runs ##########################################
##############################################################################

### https://tensorflow.rstudio.com/tools/training_flags.html
# ANN TRAINING with tfruns
library(tfruns)
library(tfestimators)

### RANDOM GRID SEARCH
#Flag values to be tested have to be defined below

# 5 Hidden Layers
runs <- tuning_run("7_MA_ANN_trainscript_5HL.R", flags = list(  #Non-Random Grid Search
  nHL = c(5),  # ONLY FOR DOCUMENTATION PURPOSES in the results table. Layers have to be changed in training script manually!
  nnodes = c(32, 64, 128),  #Nr. of nodes for hidden layer 1
  actHL = c('relu'),  #activation function for hidden layer 1
  droprate = c(0.1, 0.2, 0.3),
  actOL = c('softmax'),  #activation function for output layer
  lr = c(0.001),  #learning rate
  comp_loss = c('binary_crossentropy'),  #compiler: loss function
  comp_metric = c('accuracy'),  #compiler: metric
  epochsMax = c(400),  #Nr. of epochs max.
  batch_size = c(64, 128),  #batch size
  earlyStop_monitor = c("val_loss"),
  earlyStop_min_delta = c(0),
  earlyStop_patience = c(50),
  restore_best_weights = c(TRUE)
))

# 4 Hidden Layers
runs <- tuning_run("7_MA_ANN_trainscript_4HL.R", flags = list(  #Non-Random Grid Search
  nHL = c(4),  # ONLY FOR DOCUMENTATION PURPOSES in the results table. Layers have to be changed in training script manually!
  nnodes = c(32, 64, 128),  #Nr. of nodes for hidden layer 1
  actHL = c('relu'),  #activation function for hidden layer 1
  droprate = c(0.1, 0.2, 0.3),
  actOL = c('softmax'),  #activation function for output layer
  lr = c(0.001),  #learning rate
  comp_loss = c('binary_crossentropy'),  #compiler: loss function
  comp_metric = c('accuracy'),  #compiler: metric
  epochsMax = c(400),  #Nr. of epochs max.
  batch_size = c(64, 128),  #batch size
  earlyStop_monitor = c("val_loss"),
  earlyStop_min_delta = c(0),
  earlyStop_patience = c(50),
  restore_best_weights = c(TRUE)
))

# 3 Hidden Layers
runs <- tuning_run("7_MA_ANN_trainscript_3HL.R", flags = list(  #Non-Random Grid Search
  nHL = c(3),  # ONLY FOR DOCUMENTATION PURPOSES in the results table. Layers have to be changed in training script manually!
  nnodes = c(32, 64, 128),  #Nr. of nodes for hidden layer 1
  actHL = c('relu'),  #activation function for hidden layer 1
  droprate = c(0.1, 0.2, 0.3),
  actOL = c('softmax'),  #activation function for output layer
  lr = c(0.001),  #learning rate
  comp_loss = c('binary_crossentropy'),  #compiler: loss function
  comp_metric = c('accuracy'),  #compiler: metric
  epochsMax = c(400),  #Nr. of epochs max.
  batch_size = c(64, 128),  #batch size
  earlyStop_monitor = c("val_loss"),
  earlyStop_min_delta = c(0),
  earlyStop_patience = c(50),
  restore_best_weights = c(TRUE)
))

# 2 Hidden Layers
runs <- tuning_run("7_MA_ANN_trainscript_2HL.R", flags = list(  #Non-Random Grid Search
  nHL = c(2),  # ONLY FOR DOCUMENTATION PURPOSES in the results table. Layers have to be changed in training script manually!
  nnodes = c(32, 64, 128),  #Nr. of nodes for hidden layer 1
  actHL = c('relu'),  #activation function for hidden layer 1
  droprate = c(0.1, 0.2, 0.3),
  actOL = c('softmax'),  #activation function for output layer
  lr = c(0.001),  #learning rate
  comp_loss = c('binary_crossentropy'),  #compiler: loss function
  comp_metric = c('accuracy'),  #compiler: metric
  epochsMax = c(400),  #Nr. of epochs max.
  batch_size = c(64, 128),  #batch size
  earlyStop_monitor = c("val_loss"),
  earlyStop_min_delta = c(0),
  earlyStop_patience = c(50),
  restore_best_weights = c(TRUE)
))




##############################################################################
######## Extract results as csv file #########################################
##############################################################################

# Create data frame with all run results & parameters
results <- ls_runs()
results$model <- NULL
results$output <- NULL
#save results as csv -> specify the name of the csv file
write.csv(results, file = "_results.csv")#, row.names=FALSE)

# display runs in tensorboard
tensorboard(ls_runs())




