### Start separate data cleaning script
setwd("C:/Users/phili/Documents/_Privat/Master/WU Wien MSc Marketing/4 - Master Thesis/Code")
source("1-MA_Data Cleaning_NEW.R", echo=TRUE)



### Describe distributions in dataset

#area
ggplot(data = telco, aes(x = area, na.rm = TRUE)) + 
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  ylim(0, 12000) +
  ylab("Count") +
  xlab("Geographical Area [area]")

#income
income.prop <- as.data.frame(prop.table(table(telco$income)))
ggplot(data = telco, aes(x = income, na.rm = TRUE)) + 
  geom_bar() +
  geom_vline(xintercept = 0.5, color="red", size = 1, linetype="dashed") +
  geom_vline(xintercept = 3.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 2, y = 22500, label = "Low", color="red", size = 4) +
  annotate("text", x = 2, y = 20000, color="red", size = 4,  
           label = percent(round(income.prop$Freq[Var1=1]+income.prop$Freq[Var1=2]+income.prop$Freq[Var1=3], digits=4))) +
  geom_vline(xintercept = 6.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 5, y = 22500, label = "Medium", color="red", size = 4) +
  annotate("text", x = 5, y = 20000, color="red", size = 4,  
           label = percent(round(income.prop$Freq[Var1=4]+income.prop$Freq[Var1=5]+income.prop$Freq[Var1=6], digits=4))) +
  geom_vline(xintercept = 9.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 8, y = 22500, label = "High", color="red", size = 4) +
  annotate("text", x = 8, y = 20000, color="red", size = 4,  
           label = percent(round(income.prop$Freq[Var1=7]+income.prop$Freq[Var1=8]+income.prop$Freq[Var1=9], digits=4))) +
  ylab("Count") +
  xlab("Income Category [income]")

#months --> tenure
ggplot(data = telco, aes(x = months, na.rm = TRUE)) + 
  geom_bar() +
  geom_vline(xintercept = 12.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 15, y = 8000, label = "1st yr", color="red", size = 4) +
  geom_vline(xintercept = 24.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 27, y = 8000, label = "2nd yr", color="red", size = 4) +
  geom_vline(xintercept = 36.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 39, y = 8000, label = "3rd yr", color="red", size = 4) +
  geom_vline(xintercept = 48.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 51, y = 8000, label = "4th yr", color="red", size = 4) +
  ylab("Count") +
  xlab("Tenure (months) [months]")

#Mean monthly revenue
summary(telco$rev_Mean)

#Equipment age
ggplot(data = telco, aes(x = eqpdays, na.rm = TRUE)) + 
  geom_bar() +
  geom_vline(xintercept = 1*365+0.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 1*365+50, y = 325, label = "1 yr", color="red", size = 4) +
  geom_vline(xintercept = 2*365+0.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 2*365+50, y = 325, label = "2 yrs", color="red", size = 4) +
  geom_vline(xintercept = 3*365+0.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 3*365+50, y = 325, label = "3 yrs", color="red", size = 4) +
  geom_vline(xintercept = 4*365+0.5, color="red", size = 1, linetype="dashed") +
  annotate("text", x = 4*365+50, y = 325, label = "4 yrs", color="red", size = 4) +
  ylab("Count") +
  xlab("Equipment Age (days) [eqpdays]")

#custcare_Mean - Mean number of customer care calls
summary(telco$custcare_Mean)




### Churn & selected variables

#area barplot
prop.table(table(telco[,c("area", "churn")]),1) #show relative frequencies for area and churn
ggplot(data = telco, aes(x = area, fill = churn, na.rm = TRUE)) + 
  geom_bar(position = "fill", na.rm = TRUE) +
  scale_fill_manual("Churn", values = c("1" = "red", "0" = "green")) +
  scale_y_continuous("Percentage of Churn", label = percent) +
  scale_x_discrete("Geographical Area [area]") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +
  geom_hline(yintercept = 0.5, linetype="dashed")

#income barplot
prop.table(table(telco[,c("income", "churn")]),1) #show relative frequencies for area and churn
ggplot(data = telco, aes(x = income, fill = churn, na.rm = TRUE)) + 
  geom_bar(position = "fill", na.rm = TRUE) +
  scale_fill_manual("Churn", values = c("1" = "red", "0" = "green")) +
  scale_y_continuous("Percentage of Churn", label = percent) +
  scale_x_discrete("Income Category [income]") +
  geom_hline(yintercept = 0.5, linetype="dashed")

#months boxplot
ggplot(telco, aes(x=churn, y=months))+
  geom_boxplot(aes(x=churn, y=months), width=0.95) +
  scale_y_continuous("Mean Montly Revenue (US$)") +
  scale_x_discrete("Churn")

#Mean monthly revenue
aggregate(telco$rev_Mean, list(telco$churn), mean)

#eqpdays boxplot
ggplot(telco, aes(x=churn, y=eqpdays))+
  geom_boxplot(aes(x=churn, y=eqpdays), width=0.95) +
  scale_y_continuous("Equipment Age (days)") +
  scale_x_discrete("Churn")

#custcare_Mean - Mean number of customer care calls
aggregate(telco$custcare_Mean, list(telco$churn), mean)

