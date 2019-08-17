library(xlsx)
library(plyr)
library(dplyr)
library(psych)
library(rpart)
library(rpart.plot)
library(ROCR)
library(heuristica)
library(caret)
library(e1071)
library(lift)
library(gains)
library(C50)

data<-read.xlsx("data.xls",sheetIndex = 1)
summary(data)

data_t <- apply(data[,c(-1,-3,-11,-14,-23,-27,-29)],2,as.factor)

# C:  to refer to specific column in the data ----data[, c("RESPONSE")]
summary(as.factor(data[, c("RESPONSE")]))

# calculating missing values
summary(data[,c(23)])

#identifying the count of 0 and 1 associated with missing values
summary(as.factor(data[is.na(data[,c(23)])==1,c(32)]))

#plotting a stacked bar graph by grouping AGE against RESPONSE values

b <- c(-Inf,10,20,30,40,50,60,70,Inf) #creating AGE band with 10 as width
names <- c(10,20,30,40,50,60,70,80) #labelling every band

data_age <- cut(data[,c(23)],b,names) #create a new vector data_age and classifying the age with labels

counts <- table(data$RESPONSE,data_age)
barplot(counts,legend=rownames(counts))

#plotting a stacked bar graph by grouping DURATION against RESPONSE values

b <- c(-Inf,10,20,30,40,50,60,70,Inf) #creating DURATION band with 10 as width
names <- c(10,20,30,40,50,60,70,80) #labelling every band

data_duration <- cut(data[,c(3)],b,names) #create a new vector data_duration and classifying the duration with labels

counts <- table(data$RESPONSE,data_duration)
barplot(counts,legend=rownames(counts))

#plotting a stacked bar graph by grouping AMOUNT against RESPONSE values

b <- c(-Inf,2000,4000,6000,8000,10000,12000,14000,16000,18000,Inf) #creating AMOUNT band with 2000 as width
names <- c(2000,4000,6000,8000,10000,12000,14000,16000,18000,20000) #labelling every band

data_amount <- cut(data[,c(11)],b,names) #create a new vector data_amount and classifying the amount with labels

counts <- table(data$RESPONSE,data_amount)
barplot(counts,legend=rownames(counts))

# Mean & Std. deviation of continuous variables
data_num <- describe(data[,c(3,11,14,23,27,29)])

# Frequency of continuous variables
data_factor <-  apply(data[,c(-1,-3,-11,-14,-23,-27,-29)],2,count)


#bar plot for categorical variable
counts <- table(data$RESPONSE,data$CHK_ACCT)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$HISTORY)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$NEW_CAR)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$USED_CAR)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$FURNITURE)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$RADIO.TV)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$EDUCATION)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$RETRAINING)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$SAV_ACCT)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$EMPLOYMENT)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$MALE_DIV)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$MALE_SINGLE)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$MALE_MAR_or_WID)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$CO.APPLICANT)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$GUARANTOR)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$PRESENT_RESIDENT)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$REAL_ESTATE)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$PROP_UNKN_NONE)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$OTHER_INSTALL)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$RENT)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$OWN_RES)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$JOB)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$TELEPHONE)
barplot(counts,legend=rownames(counts))

counts <- table(data$RESPONSE,data$FOREIGN)
barplot(counts,legend=rownames(counts))

#Decision Tree for full data

data_t2 <- cbind(data_t,data[,c(3,11,14,23,27,29)]) 

dt_1 <- rpart(RESPONSE ~ .,data = data_t2,method = "class", parms = list(split = "gini"),
              control = rpart.control(minsplit = 50, minbucket = round(50/3), cp = 0, maxcompete = 4, 
                                      maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0, maxdepth = 30))

prp(dt_1)
rpart.plot(dt_1)

predict_0 <- predict(dt_1,data_t2[,c(-25)])

colnames(predict_0) <- c("c_0","c_1")

predict_0_check <- predict_0 %>% 
  as.data.frame() %>% 
  mutate(value = ifelse(c_1 > 0.5,1,0))

temp0 <- prediction(predict_0_check[,c(3)],data_t2[,c(25)])

predict_0_perf1 <- performance(temp0,"tpr","fpr") #ROC curve
plot(predict_0_perf1)

predict_0_check[,c(3)] <- as.factor(predict_0_check[,c(3)])

confusionMatrix(predict_0_check[,c(3)],data_t2[,c(25)]) #Confusion Matrix

plotLift(predict_0_check[,c(3)],data_t2[,c(25)]) #Lift Chart

#Building & measuring decision tree with 50% data

train_data_t2 <- sample_frac(data_t2, 0.7)
test_data_t2 <- setdiff(data_t2,train_data_t2)
  
dt_2 <- rpart(RESPONSE ~ .,data = train_data_t2,method = "class", parms = list(split = "gini"),
              control = rpart.control(minsplit = 50, minbucket = round(50/3), cp = 0, maxcompete = 4, 
                                      maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0, maxdepth = 30))


predict_1 <- predict(dt_2,test_data_t2[,c(-25)])

colnames(predict_1) <- c("c_0","c_1")
predict_1_check <- predict_1 %>% 
  as.data.frame() %>% 
  mutate(value = ifelse(c_1 > 0.5,1,0))


temp <- prediction(predict_1_check[,c(3)],test_data_t2[,c(25)])

predict_1_perf1 <- performance(temp,"tpr","fpr") #ROC curve
plot(predict_1_perf1)

predict_1_check[,c(3)] <- as.factor(predict_1_check[,c(3)])

confusionMatrix(predict_1_check[,c(3)],test_data_t2[,c(25)]) #Confusion Matrix

plotLift(predict_1_check[,c(3)],test_data_t2[,c(25)]) #Lift Chart

t1 <- as.numeric(predict_1_check[,c(3)])
t2 <- as.numeric(test_data_t2[,c(25)])

#t3 <- gains(t2,t1)
#plot (t3)
#print (t3)

#Plotting DT using C5.0

train_data_t21 <- train_data_t2 %>%
  mutate (NEW_AGE = ifelse(is.na(AGE),0, AGE))

train_data_t21$AGE <- NULL

#train_data_t21$NEW_AGE <- as.numeric(train_data_t21$NEW_AGE)

train_data_t21$NEW_CAR <- as.character(train_data_t21$NEW_CAR)
train_data_t21$NEW_CAR [train_data_t21$NEW_CAR == ""] <- "missing"
train_data_t21$NEW_CAR <- as.factor(train_data_t21$NEW_CAR)

train_data_t21$USED_CAR <- as.character(train_data_t21$USED_CAR)
train_data_t21$USED_CAR [train_data_t21$USED_CAR == ""] <- "missing"
train_data_t21$USED_CAR <- as.factor(train_data_t21$USED_CAR)

train_data_t21$FURNITURE <- as.character(train_data_t21$FURNITURE)
train_data_t21$FURNITURE [train_data_t21$FURNITURE == ""] <- "missing"
train_data_t21$FURNITURE <- as.factor(train_data_t21$FURNITURE)

train_data_t21$RADIO.TV <- as.character(train_data_t21$RADIO.TV)
train_data_t21$RADIO.TV [train_data_t21$RADIO.TV == ""] <- "missing"
train_data_t21$RADIO.TV <- as.factor(train_data_t21$RADIO.TV)

train_data_t21$EDUCATION <- as.character(train_data_t21$EDUCATION)
train_data_t21$EDUCATION [train_data_t21$EDUCATION == ""] <- "missing"
train_data_t21$EDUCATION <- as.factor(train_data_t21$EDUCATION)

train_data_t21$RETRAINING <- as.character(train_data_t21$RETRAINING)
train_data_t21$RETRAINING [train_data_t21$RETRAINING == ""] <- "missing"
train_data_t21$RETRAINING <- as.factor(train_data_t21$RETRAINING)

C50_tree <- C5.0(x = train_data_t21[,c(-25)], y = train_data_t2$RESPONSE,
                 control = C5.0Control(winnow = FALSE, CF = 0.25))
summary(C50_tree)

C5imp(C50_tree)

plot(C50_tree)

test_data_t21 <- test_data_t2 %>%
  mutate (NEW_AGE = ifelse(is.na(AGE),0, AGE))

test_data_t21$AGE <- NULL

#test_data_t21$NEW_AGE <- as.numeric(test_data_t21$NEW_AGE)

test_data_t21$NEW_CAR <- as.character(test_data_t21$NEW_CAR)
test_data_t21$NEW_CAR [test_data_t21$NEW_CAR == ""] <- "missing"
test_data_t21$NEW_CAR <- as.factor(test_data_t21$NEW_CAR)

test_data_t21$USED_CAR <- as.character(test_data_t21$USED_CAR)
test_data_t21$USED_CAR [test_data_t21$USED_CAR == ""] <- "missing"
test_data_t21$USED_CAR <- as.factor(test_data_t21$USED_CAR)

test_data_t21$FURNITURE <- as.character(test_data_t21$FURNITURE)
test_data_t21$FURNITURE [test_data_t21$FURNITURE == ""] <- "missing"
test_data_t21$FURNITURE <- as.factor(test_data_t21$FURNITURE)

test_data_t21$RADIO.TV <- as.character(test_data_t21$RADIO.TV)
test_data_t21$RADIO.TV [test_data_t21$RADIO.TV == ""] <- "missing"
test_data_t21$RADIO.TV <- as.factor(test_data_t21$RADIO.TV)

test_data_t21$EDUCATION <- as.character(test_data_t21$EDUCATION)
test_data_t21$EDUCATION [test_data_t21$EDUCATION == ""] <- "missing"
test_data_t21$EDUCATION <- as.factor(test_data_t21$EDUCATION)

test_data_t21$RETRAINING <- as.character(test_data_t21$RETRAINING)
test_data_t21$RETRAINING [test_data_t21$RETRAINING == ""] <- "missing"
test_data_t21$RETRAINING <- as.factor(test_data_t21$RETRAINING)

predict_2 <- predict(C50_tree,test_data_t21[,c(-25)])

predict_2_tmp <- as.data.frame(predict_2)
predict_2_tmp[,c(1)] <- as.numeric(as.character(predict_2_tmp[,c(1)]))

temp1 <- prediction(predict_2_tmp[,c(1)],test_data_t21[,c(25)])

predict_2_perf1 <- performance(temp1,"tpr","fpr") #ROC curve
plot(predict_2_perf1)

predict_2_tmp[,c(1)] <- as.factor(predict_2_tmp[,c(1)])

confusionMatrix(predict_2_tmp[,c(1)],test_data_t2[,c(25)]) #Confusion Matrix

plotLift(predict_2_tmp[,c(1)],test_data_t2[,c(25)]) #Lift Chart

#################################################################