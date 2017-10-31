---
title: "Loan Prediction"
author: "Rohit Dixit"
output:
  word_document: default
  pdf_document: default
  html_document: default
---

## Problem Statement

Company wants to automate the loan eligibility process (real time) based on customer detail provided while filling online application form. These details are Gender, Marital Status, Education, Number of Dependents, Income, Loan Amount, Credit History and others. To automate this process, they have given a problem to identify the customers segments, those are eligible for loan amount so that they can specifically target these customers. Here they have provided a partial data set.

## Data Glimpse
**Variable**	        **Description**    
Loan_ID	           -Unique Loan ID   
Gender	           -Male/ Female   
Married	           -Applicant married (Y/N)   
Dependents	       -Number of dependents    
Education	         -Applicant Education (Graduate/ Under Graduate)    
Self_Employed      -Self employed (Y/N)    
ApplicantIncome	   -Applicant income    
CoapplicantIncome  -Coapplicant income    
LoanAmount	       -Loan amount in thousands    
Loan_Amount_Term	 -Term of loan in months     
Credit_History	   -credit history meets guidelines    
Property_Area	     -Urban/ Semi Urban/ Rural    
Loan_Status	Loan   -approved (Y/N)    

## R Code
**Importing Library**
library(caret)
library(mlbench)
library(ggplot2)
library(ggthemes)
library(plyr)
library(RANN)
library(gridExtra)

**Data Importing**
train_data <-read.csv("train.csv")
test_data <-read.csv("test.csv")
**Data Exploration**
summary(train_data)
summary(test_data)
The summary shows, we have NA values to handle let's explore our data more    

## Data Visualisation

#Checking our target variable- **Loan_Status**
table(train_data$Loan_Status)
barplot(table(train_data$Loan_Status),main= "Loan_Status")

Lets's Explore our Independent variables-    
1. Gender
plot1 <- ggplot(data=train_data, aes(train_data$Gender))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Gender")+labs(title="Bar Plot of Gender on Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Gender))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Gender")+labs(title="Bar Plot of Gender on Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

2. Married
plot1 <- ggplot(data=train_data, aes(train_data$Married))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Married")+labs(title="Bar Plot of Married on Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Married))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Married")+labs(title="Bar Plot of Married on Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

3. Dependents
plot1 <- ggplot(data=train_data, aes(train_data$Dependents))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Dependents")+labs(title="Bar Plot of Dependents on Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Dependents))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Dependents")+labs(title="Bar Plot of Dependents on Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

4. Education
plot1 <- ggplot(data=train_data, aes(train_data$Education))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Education")+labs(title="Bar Plot of Education on Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Education))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Education")+labs(title="Bar Plot of Education on Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

5. Self_Employed
plot1 <- ggplot(data=train_data, aes(train_data$Self_Employed))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Self_Employed")+labs(title="Bar Plot of Self_Employed in Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Self_Employed))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Self_Employed")+labs(title="Bar Plot of Self_Employed in Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

6. ApplicantIncome (Numeric) & CoapplicantIncome (Numeric)
par(mfrow=c(1,2))
boxplot(train_data$ApplicantIncome,train_data$CoapplicantIncome,names=c("Applicant Income","Coapplicant Income"),main="Box Plot of Applicant & Coapplicant Income on Train Data")
boxplot(test_data$ApplicantIncome,test_data$CoapplicantIncome,names=c("Applicant Income","Coapplicant Income"),main="Box Plot of Applicant & Coapplicant Income on Test Data")

8. LoanAmount (Numeric)
par(mfrow=c(1,2))
boxplot(train_data$LoanAmount,main="Box Plot of LoanAmount on Train set")
boxplot(test_data$LoanAmount,main="Box Plot of LoanAmount on Test set")

9. Loan_Amount_Term (Numeric)
plot1 <- ggplot(data=train_data, aes(train_data$Loan_Amount_Term))+geom_histogram(col="black",fill="steelblue",alpha=1)+theme_minimal()+labs(x = "Loan_Amount_Term (Train Data)")+labs(title= "Histogram of Loan_Amount_Term")
plot2 <- ggplot(data=test_data, aes(test_data$Loan_Amount_Term))+geom_histogram(col="black",fill="steelblue",alpha=1)+theme_minimal()+labs(x = "Loan_Amount_Term (Test Data)")+labs(title= "Histogram of Loan_Amount_Term")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

10. Credit_History (Factor)
plot1 <- ggplot(data=train_data, aes(train_data$Credit_History))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Credit_History")+labs(title="Bar Plot of Credit_History in Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Credit_History))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Credit_History")+labs(title="Bar Plot of Credit_History in Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

11. Property_Area
plot1 <- ggplot(data=train_data, aes(train_data$Property_Area))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Property_Area")+labs(title="Bar Plot of Property_Area in Train Data")
plot2 <- ggplot(data=test_data, aes(test_data$Property_Area))+geom_bar(fill="steelblue")+theme_minimal()+labs(x = "Property_Area")+labs(title="Bar Plot of Property_Area in Test Data")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

12.Multiple Plots
theme_set(theme_economist())
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))
print(ggplot(train_data, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))
print(ggplot(train_data, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status by Applicant income"))
print(ggplot(train_data, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))
print(ggplot(train_data, aes(x=Loan_Status,y=LoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))

** Filling NA values**
#Usually while applying for loan total family income is calculated
So we can add Applicant and Co_Applicant income, but before that we need to combine our dataset
Complete_Data <- rbind(train_data[,2:12],test_data[,2:12])
Complete_Data <- mutate(Complete_Data,TotalIncome=ApplicantIncome+CoapplicantIncome)

#When there is "No" co-applicant income assuming as Unmarried and Married otherwise
Complete_Data$Gender <- as.character(Complete_Data$Gender)
Complete_Data$Married <- as.character(Complete_Data$Married)
Complete_Data$Self_Employed <- as.character(Complete_Data$Self_Employed)
Complete_Data$Married[Complete_Data$Married=="" & Complete_Data$CoapplicantIncome==0]<-"No"
Complete_Data$Married[Complete_Data$Married==""]<- "Yes"

#Plots show that if gender is male its income is more than female so

Complete_Data$Dependents <- as.character(Complete_Data$Dependents)
Complete_Data$Gender[Complete_Data$Gender=="" & Complete_Data$Dependents==""] <- "Male"

#When Dependents is unknown but not married then assuming no dependents
Complete_Data$Dependents[Complete_Data$Dependents=="" & Complete_Data$Married=="No"]<- "0"

#Most of the loan term is 360, so filling NA as 360 in loan amount and renaming 350 as 360 and 6 as 60 since their feq is low and m,ight be due to tying error while entering data
Complete_Data$Loan_Amount_Term[is.na(Complete_Data$Loan_Amount_Term)]<-"360"
library(car)
Complete_Data$Loan_Amount_Term <- recode(Complete_Data$Loan_Amount_Term,"'350'='360';'6'='60'")

#Assuming "" empty factor in self employed. As most are not self employed
Complete_Data$Self_Employed[Complete_Data$Self_Employed==""] <- "No"

#Assuming person with no credit history as another catrgory
Complete_Data$Credit_History<-recode(Complete_Data$Credit_History,"NA=2")
Complete_Data$Gender <- as.factor(Complete_Data$Gender)
Complete_Data$Married <- as.factor(Complete_Data$Married)
Complete_Data$Dependents <- as.factor(Complete_Data$Dependents)
Complete_Data$Self_Employed <- as.factor(Complete_Data$Self_Employed)
Complete_Data$Loan_Amount_Term <- as.factor(Complete_Data$Loan_Amount_Term)

#To predict Remaining Gender by (Mode Imputation) & Dependents
levels(Complete_Data$Gender)[levels(Complete_Data$Gender)==""] <- "Male"
levels(Complete_Data$Dependents)[levels(Complete_Data$Dependents)==""] <- "0"

#We will predict Loan Amount using K-Nearrest neighbours
preProcValues <- preProcess(Complete_Data, method = c("knnImpute","center","scale"))
Complete_data_processed <- predict(preProcValues, Complete_Data)

#Splitting Training and Test Data set back
trainSet <- Complete_data_processed[ 1:614,]
trainSet$Loan_Status <- train_data$Loan_Status
testSet <- Complete_data_processed[615:981,]

#Creating Train control
fitControl <- trainControl(method = "cv", number = 7, savePredictions = 'final', classProbs = T)

Creating Random Forest

#Training the random forest model
model_rf<-train(Loan_Status~.,data=trainSet,method='rf',trControl=fitControl,tuneLength=3)
#Predicting using random forest model
testSet$pred_rf<-predict(object = model_rf,testSet[,-13])

This was the uploaded to kaggle and output accuracy was 81.1% after tuning random forest.
