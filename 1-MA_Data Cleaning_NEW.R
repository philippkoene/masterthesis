library(Hmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(caret)
library(psych)
library(reshape)
library(tidyr)


########################################
######## DATA IMPORT & CLEANING ########
########################################

### IMPORT DATA
telco <- read.csv(file = "C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/data/Telecom_customer churn.csv", 
                  header = TRUE, na.strings=c(""))

# Convert Variables
telco$churn <- as.factor(telco$churn) #convert to factor
telco$income <- as.factor(telco$income) #convert to factor
telco$rv <- as.factor(telco$rv) #convert to factor
telco$truck <- as.factor(telco$truck) #convert to factor
telco$forgntvl <- as.factor(telco$forgntvl) #convert to factor
telco$creditcd <- recode(telco$creditcd, Y="1", N="0") #replace Y/N by 1/0, rest = N/V
telco$new_cell <- recode(telco$new_cell, Y="1", N="0") #replace Y/N by 1/0, rest = NA
telco$new_cell <- na_if(telco$new_cell, "U")
telco$new_cell <- as.character(telco$new_cell)
telco$new_cell <- as.factor(telco$new_cell)
telco$asl_flag <- recode(telco$asl_flag, Y="1", N="0") #replace Y/N by 1/0, rest = N/V
telco$hnd_webcap <- na_if(telco$hnd_webcap, "NA")
telco$hnd_webcap <- as.character(telco$hnd_webcap)
telco$hnd_webcap <- as.factor(telco$hnd_webcap)
telco$kid11_15 <- recode(telco$kid11_15, Y="1", U="0") #replace Y/U by 1/0
telco$kid16_17 <- recode(telco$kid16_17, Y="1", U="0") #replace Y/U by 1/0
telco$kid6_10 <- recode(telco$kid6_10, Y="1", U="0") #replace Y/U by 1/0
colnames(telco)[colnames(telco)=="refurb_new"] <- "refurb"
telco$refurb <- recode(telco$refurb, R="1", N="0")

# Reorder 
telco <- telco[c(100,52,85,72,54,98,90,83,92,91,89,87,86,93,96,97,94,95,82,84,53,88,81,71,80,79,
                 51,55,73,99,75,78,77,50,2,76,74,1,3,60,61,59,45,65,66,67,68,69,70,63,64,62,15,14,
                 47,48,26,25,10,11,23,22,46,24,4,8,44,13,12,27,34,30,29,43,42,39,38,31,35,33,41,40,
                 5,6,32,37,36,19,18,21,20,9,28,56,57,58,17,16,7,49)]

# Delete Customer ID
telco$Customer_ID <- NULL


# Recode numerical features with <26 unique values to factors
nums <- unlist(lapply(telco, is.numeric)) #get all numerical features in original dataset
telco.num <- telco[,nums] #create a subset with those numerical features
uniqueval <- as.data.frame(sapply(telco.num, function(x) length(unique(x))))
library(tibble)
uniqueval <- uniqueval %>% rownames_to_column("Var") #copy rownames to own column
colnames(uniqueval)[colnames(uniqueval)=="sapply(telco.num, function(x) length(unique(x)))"] <- "n"  #rename column
uniqueval <- uniqueval[order(uniqueval$"n"),]  #order by nr. of unique values
uniqueval <- subset(uniqueval, uniqueval[,2] <26)  #create subset with variables having >5,000 NAs
uniqueval

table(telco$numbcars)  #count of each unique value
telco$numbcars <- as.factor(telco$numbcars)  #convert to factor
#keep all 3 levels
table(telco$numbcars)  #check the count of each unique value

table(telco$adults)  #count of each unique value
telco$adults <- as.factor(telco$adults)  #convert to factor
levels(telco$adults) <- c("1","2","3","4","5+","5+")  #combine & recode factor levels
table(telco$adults)  #check the count of each unique value

table(telco$actvsubs)  #count of each unique value
telco$actvsubs <- as.factor(telco$actvsubs)  #convert to factor
levels(telco$actvsubs) <- c("0","1","2","3","4","5+","5+","5+","5+","5+","5+","5+")  #combine & recode factor levels
table(telco$actvsubs)  #check the count of each unique value

table(telco$uniqsubs)  #count of each unique value
telco$uniqsubs <- as.factor(telco$uniqsubs)  #convert to factor
levels(telco$uniqsubs) <- c("1","2","3","4","5+","5+","5+","5+","5+","5+","5+","5+","5+","5+","5+")  #combine & recode factor levels
table(telco$uniqsubs)  #check the count of each unique value

table(telco$models)  #count of each unique value
telco$models <- as.factor(telco$models)  #convert to factor
levels(telco$models) <- c("1","2","3","4","5+","5+","5+","5+","5+","5+","5+","5+","5+","5+")  #combine & recode factor levels
table(telco$models)  #check the count of each unique value

table(telco$lor)  #count of each unique value
telco$lor <- as.factor(telco$lor)  #convert to factor
levels(telco$lor) <- c("0-3","0-3","0-3","0-3","4-7","4-7","4-7","4-7","8-11","8-11","8-11","8-11",
                       "12-15","12-15","12-15","12-15")  #combine & recode factor levels
table(telco$lor)  #check the count of each unique value

table(telco$hnd_price)  #count of each unique value
telco$hnd_price <- as.factor(telco$hnd_price)  #convert to factor
levels(telco$hnd_price) <- c("0-50","0-50","0-50","51-100","51-100","51-100","101-150","101-150",
                             "101-150","151+","151+","151+","151+","151+","151+","151+","151+")  #combine & recode factor levels
table(telco$hnd_price)  #check the count of each unique value

table(telco$phones)  #count of each unique value
telco$phones <- as.factor(telco$phones)  #convert to factor
levels(telco$phones) <- c("1","2","3","4","5+","5+","5+","5+","5+","5+","5+","5+","5+","5+","5+","5+",
                          "5+","5+","5+","5+","5+","5+","5+","5+")  #combine & recode factor levels
table(telco$phones)  #check the count of each unique value


# Categorical variables: add NA as own factor level
factors <- unlist(lapply(telco, is.factor))  #get all categorical features in original dataset
telco.cat <- telco[,factors]  #create a subset with those categorical features
telco.cat$churn <- NULL  #exclude churn from process
mvCat <- sapply(telco.cat, function(x) sum(is.na(x)))  #how many N/As does every categorical variable contain
mvCat <- data.frame(var = names(mvCat), NA_count = mvCat)  #convert to dataframe
rownames(mvCat) <- NULL  #remove row names
mvCat <- mvCat[order(-mvCat$NA_count),]  #order by Nr. of NAs (descending)
mvCat <- subset(mvCat, mvCat[,2] >0)  #create subset with categorical variables containing NAs
mvCat <- as.vector(mvCat$var)
mvCat

telco$new_cell <- factor(telco$new_cell, labels=c(levels(telco$new_cell),"NA"), exclude = NULL)
telco$numbcars <- factor(telco$numbcars, labels=c(levels(telco$numbcars),"NA"), exclude = NULL)
telco$dwllsize <- factor(telco$dwllsize, labels=c(levels(telco$dwllsize),"NA"), exclude = NULL)
telco$HHstatin <- factor(telco$HHstatin, labels=c(levels(telco$HHstatin),"NA"), exclude = NULL)
telco$ownrent <- factor(telco$ownrent, labels=c(levels(telco$ownrent),"NA"), exclude = NULL)
telco$dwlltype <- factor(telco$dwlltype, labels=c(levels(telco$dwlltype),"NA"), exclude = NULL)
telco$lor <- factor(telco$lor, labels=c(levels(telco$lor),"NA"), exclude = NULL)
telco$income <- factor(telco$income, labels=c(levels(telco$income),"NA"), exclude = NULL)
telco$adults <- factor(telco$adults, labels=c(levels(telco$adults),"NA"), exclude = NULL)
telco$infobase <- factor(telco$infobase, labels=c(levels(telco$infobase),"NA"), exclude = NULL)
telco$hnd_webcap <- factor(telco$hnd_webcap, labels=c(levels(telco$hnd_webcap),"NA"), exclude = NULL)
telco$prizm_social_one <- factor(telco$prizm_social_one, labels=c(levels(telco$prizm_social_one),"NA"), exclude = NULL)
telco$creditcd <- factor(telco$creditcd, labels=c(levels(telco$creditcd),"NA"), exclude = NULL)
telco$ethnic <- factor(telco$ethnic, labels=c(levels(telco$ethnic),"NA"), exclude = NULL)
telco$forgntvl <- factor(telco$forgntvl, labels=c(levels(telco$forgntvl),"NA"), exclude = NULL)
telco$kid0_2 <- factor(telco$kid0_2, labels=c(levels(telco$kid0_2),"NA"), exclude = NULL)
telco$kid11_15 <- factor(telco$kid11_15, labels=c(levels(telco$kid11_15),"NA"), exclude = NULL)
telco$kid16_17 <- factor(telco$kid16_17, labels=c(levels(telco$kid16_17),"NA"), exclude = NULL)
telco$kid3_5 <- factor(telco$kid3_5, labels=c(levels(telco$kid3_5),"NA"), exclude = NULL)
telco$kid6_10 <- factor(telco$kid6_10, labels=c(levels(telco$kid6_10),"NA"), exclude = NULL)
telco$marital <- factor(telco$marital, labels=c(levels(telco$marital),"NA"), exclude = NULL)
telco$rv <- factor(telco$rv, labels=c(levels(telco$rv),"NA"), exclude = NULL)
telco$truck <- factor(telco$truck, labels=c(levels(telco$truck),"NA"), exclude = NULL)
telco$hnd_price <- factor(telco$hnd_price, labels=c(levels(telco$hnd_price),"NA"), exclude = NULL)
telco$area <- factor(telco$area, labels=c(levels(telco$area),"NA"), exclude = NULL)
telco$dualband <- factor(telco$dualband, labels=c(levels(telco$dualband),"NA"), exclude = NULL)
telco$models <- factor(telco$models, labels=c(levels(telco$models),"NA"), exclude = NULL)
telco$phones <- factor(telco$phones, labels=c(levels(telco$phones),"NA"), exclude = NULL)
telco$refurb <- factor(telco$refurb, labels=c(levels(telco$refurb),"NA"), exclude = NULL)

#same thing as for loop (however, does not work for some reason)
#for (i in 1:length(mvCat)) { 
#print(i)
#telco[,mvCat[i]] <- factor(telco[,mvCat[i]], labels=c(levels(telco[,mvCat[i]]),"NA"), exclude = NULL)
#}


# MISSING VALUES 
mv <- sapply(telco, function(x) sum(is.na(x)))  #how many N/As does every variable contain
mv <- data.frame(var = names(mv), NA_count = mv)  #convert to dataframe
rownames(mv) <- NULL  #remove row names
mv <- mv[order(-mv$NA_count),]  #order by Nr. of NAs (descending)
mv <- subset(mv, mv[,2] >5000)  #create subset with variables having >5,000 NAs
mv_list <- as.vector(mv$var)  #create list with variables to be deleted --> none 
telco <- telco[!(names(telco) %in% mv_list)]  #delete variables with >5,000 NAs
telco <- na.omit(telco) #delete observations with NAs

# Delete all unused factor levels (which contain no observation anymore after na.omit)
telco <- droplevels(telco)  # dualband, models, phones, refurb all had 0 NAs
