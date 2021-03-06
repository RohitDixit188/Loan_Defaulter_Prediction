---
title: "Loan Prediction"
author: "Rohit Dixit"
output:
  html_document:
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Problem Statement

Company wants to automate the loan eligibility process (real time) based on customer detail provided while filling online application form. These details are Gender, Marital Status, Education, Number of Dependents, Income, Loan Amount, Credit History and others. To automate this process, they have given a problem to identify the customers segments, those are eligible for loan amount so that they can specifically target these customers. Here they have provided a partial data set.

# Data Glimpse
**Variable**  | **Description**
------------- | -------------
Loan_ID    | Unique Loan ID
Gender        | Male/ Female
Married       | Applicant married (Y/N) 
Dependents        | Number of dependents 
Education        | Applicant Education (Graduate/ Under Graduate)
Self_Employed        | Self employed (Y/N) 
ApplicantIncome        | Applicant income 
CoapplicantIncome        | Coapplicant income 
LoanAmount        | Loan amount in thousands 
Loan_Amount_Term        | Term of loan in months 
Credit_History        | credit history meets guidelines 
Property_Area        | Urban/ Semi Urban/ Rural 
Loan_Status	Loan        | approved (Y/N) 


## R Code

### Importing Library
```{r,message=FALSE, warning=FALSE}
library(caret)
library(mlbench)
library(ggplot2)
library(ggthemes)
library(plyr)
library(RANN)
library(gridExtra)
library(Rmisc)
library(caTools)
library(DiagrammeR)
library(car)
library(doParallel)
library(randomForest)
```

### Importing Data  
Data available at [https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv](https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv)

```{r, message=FALSE, warning=FALSE, tidy = TRUE}
data<-read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))
```

# Data Exploration
```{r, warning=FALSE}
summary(data)
```
The summary shows, we have NA values to handle let's explore our data more.  

## Exploring target variable-**Loan_Status**
```{r, warning=FALSE,fig.width = 6}
table(data$Loan_Status)
barplot(table(data$Loan_Status),main= "Loan_Status")
```

## Exploring our Independent variables-  
### 1. Gender 

```{r, echo=FALSE, warning=FALSE}
p1 = ggplot(data=data, aes(Gender, fill=Gender))+geom_bar()+theme_minimal()+labs(x = "Gender")+labs(title="Bar Plot of Gender")+annotate("text", x = 1,y=400, label = "* Highlights:",fontface =2)+annotate("text",x = 1.38, y = 350, label = "- Majority of are Male applicants")+annotate("text",x = 1.1, y = 300, label = "- Unknwon Gender")
p2 = ggplot(data, aes(x=Loan_Status,fill=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Plot of Loan Status facet by Gender of Applicant")
multiplot(p1, p2, cols=1)
```

### 2. Married  

```{r, echo=FALSE, warning=FALSE}
p1 = ggplot(data=data, aes(Married,fill=Loan_Status))+geom_bar()+theme_minimal()+labs(x = "Married")+labs(title="Bar Plot of Married")+annotate("text", x = 1,y=400, label = "* Highlights:",fontface =2)+annotate("text",x = 1.42, y = 350, label = "- Majority of applicants are Married")+annotate("text",x = 1.44, y = 300, label = "- We have Unknwon Maratial Status")+annotate("text",x = 1.5, y = 250, label = "- More Loan Approved Entries in Data")
p2 = ggplot(data, aes(x=Loan_Status,fill=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Plot of Marital Status facet by Loan Status of Applicant")
multiplot(p1, p2, cols=1)
```

### 3. Dependents  

```{r, echo=FALSE, warning=FALSE}
p1 = ggplot(data=data, aes(Dependents,fill=Dependents))+geom_bar()+theme_minimal()+labs(x = "Dependents")+labs(title="Bar Plot of Dependents")+annotate("text", x = 3,y=400, label = "* Highlights:",fontface =2)+annotate("text",x = 4, y = 350, label = "- Majority of applicants have no dependents")+annotate("text",x = 3.65, y = 300, label = "- We have Unknwon dependents")
p2 = ggplot(data, aes(x=Loan_Status,fill=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Plot of Loan Status facet by number of Dependents of Applicant")
multiplot(p1, p2, cols=1)
```

### 4. Education  

```{r, echo=FALSE, warning=FALSE}
t=c(`N`="Loan_Approved_Status - NO", `Y`="Loan_Approved_Status - YES")
p1 = ggplot(data=data, aes(Education))+geom_bar()+facet_grid(.~Loan_Status,labeller = as_labeller(t))+theme_minimal()+labs(x = "Education")+labs(title="Bar Plot of Education")
p2 = ggplot(data, aes(x=Loan_Status,fill=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Plot of Loan Status facet by Education of Applicant")
multiplot(p1, p2, cols=1)
```

* Take Away points:
+ Majority of are Graduates   
  
  
### 5. Self_Employed  

```{r, echo=FALSE, warning=FALSE}
p1 = ggplot(data, aes(x=Loan_Status,y=Self_Employed,color=Gender,shape=Loan_Status))+geom_jitter(alpha=0.7)+ggtitle("Self Employed & Loan status Jitter Plot with Gender of Applicant")+scale_shape_manual(values = c(0,16))
p2 = ggplot(data, aes(x=Self_Employed,fill=Loan_Status))+geom_bar()+facet_grid(.~Loan_Status)+ggtitle("Plot of Loan Status facet by Employment status of Applicant")
multiplot(p1, p2, cols=1)
```

* Take Away points:
  + Majority of applicants are not self employed
  + We have Unknwon values, so preprocessing needed  
  
  
### 6. ApplicantIncome (Numeric) & CoapplicantIncome (Numeric)  

```{r, echo=FALSE, warning=FALSE}
p1 = ggplot(data, aes(x=Loan_Status,y=ApplicantIncome,fill=Loan_Status))+geom_boxplot()+ggtitle("Box plot of Applicant income and Loan Status")
p2 = ggplot(data, aes(x=Loan_Status,y=CoapplicantIncome,fill=Loan_Status))+geom_boxplot()+ggtitle("Box plot of Coapplicant income and Loan Status")
multiplot(p1, p2, cols=1)
```

* Take Away points:
  + Applicant Income box plot shows right skewness
  + We have outliers so scaling and centering willl be needed  
  
### 7. LoanAmount (Numeric)  

```{r, echo=FALSE, warning=FALSE}
print(ggplot(data, aes(x=Loan_Status,y=LoanAmount,fill=Loan_Status))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))
```

* Take Away points:  
  + Plots show right skewness   
  + We have outliers so scaling and centering willl be needed   
  
### 8. Loan_Amount_Term (Numeric)  

```{r echo=FALSE, message=FALSE, warning=FALSE}
print(ggplot(data=data, aes(data$Loan_Amount_Term))+geom_histogram(col="black",fill="steelblue",alpha=1)+theme_minimal()+labs(x = "Loan_Amount_Term")+labs(title= "Histogram of Loan_Amount_Term"))
```

* Take Away points:
  + Majority have 360 months as loan amount term
  + We might have a few typo as 350 monts and 6 months tuple are present  

### 9. Credit_History (Factor)  

```{r, echo=FALSE, warning=FALSE}
print(ggplot(data, aes(x=Loan_Status,y=Credit_History,shape=Loan_Status,color=Loan_Status,alpha=0.5))+geom_jitter()+ggtitle("Loan Status and Credit History"))
```

* Take Away points:- 
  + It should be a factor variable like yes or no etc.  
  
### 10. Property_Area  

```{r, echo=FALSE, warning=FALSE}
print(ggplot(data=data, aes(Property_Area,fill=Property_Area))+geom_bar()+theme_minimal()+labs(x = "Property_Area")+labs(title="Bar Plot of Property_Area in Data"))
```

* Take Away points:
  + Majority of property holdings are in semiurban area  

# Flow Chart of Data Imputation
```{r echo=FALSE, message=FALSE, warning=FALSE,fig.height=8}
mermaid("
graph TD
  A>Data Cleaning Pipeline Steps]-->B(When there is No co-applicant income assuming as Unmarried and Married otherwise)
  B(When there is No co-applicant income assuming as Unmarried and Married otherwise)-->C(Plot shows that if gender is male its income is more than female so will use it)
  C(Plot shows that if gender is male its income is more than female so will use it)-->D(When Dependents is unknown but not married then assuming no dependents)
  D(When Dependents is unknown but not married then assuming no dependents)-->E(Mode of the loan term is 360, so converting NA as 360, 350 to 360 and 6 to 60 assuming typing error)
  E(Mode of the loan term is 360, so converting NA as 360, 350 to 360 and 6 to 60 assuming typing error)-->F(Assuming NA factor in self employed as NO. As most are not self employed)
  F(Assuming NA factor in self employed as NO. As most are not self employed)-->G(Assuming person with no credit history as another category)
  G(Assuming person with no credit history as another category)-->H(Imputing remaining gender & dependents using Mode Imputation)
  H(Imputing remaining gender & dependents using Mode Imputation)-->I(Performing Centering Scaling on numeric data type column)
  I(Performing Centering Scaling on numeric data type column)-->J(K-Nearest Neighbours to predict unknown Loan Amount)
")
```

# Data Imputation Code
 
## Maratial Status Imputation
```{r, message=FALSE, warning=FALSE}
#When there is "No" co-applicant income assuming as Unmarried and Married otherwise 
#converting factor varibales to character later will convert back
data$Gender <- as.character(data$Gender)
data$Married <- as.character(data$Married)
data$Self_Employed <- as.character(data$Self_Employed)
data$Married[data$Married=="" & data$CoapplicantIncome==0]<-"No"
data$Married[data$Married==""]<- "Yes"
```

## Gender Imputation
```{r, message=FALSE, warning=FALSE}
#Data shows if gender is male its income is more than female so   
data$Dependents <- as.character(data$Dependents)
data$Gender[data$Gender=="" & data$Dependents==""] <- "Male"
```

## Dependents Imputation   
```{r, message=FALSE, warning=FALSE}
#When Dependents is unknown but not married then assuming no dependents 
data$Dependents[data$Dependents=="" & data$Married=="No"]<- "0"
```

## Loan Amount Term Imputation
```{r, message=FALSE, warning=FALSE}
#Most of the loan term is 360, so filling NA as 360 in loan amount and renaming 350 as 360 and 6 as 60 since their frequency is less and might be due to typing error while entering data  
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)]<-"360"
library(car)
data$Loan_Amount_Term <- recode(data$Loan_Amount_Term,"'350'='360';'6'='60'")
```

## Self Employed Imputation  
```{r, message=FALSE, warning=FALSE}
#Assuming "" empty factor in self employed. As most are not self employed
data$Self_Employed[data$Self_Employed==""] <- "No"
```

## Credit History Imputation  
```{r, message=FALSE, warning=FALSE}
#Assuming person with no credit history as another catrgory
data$Credit_History<-recode(data$Credit_History,"NA=2")
#converting all character to factor back
data$Credit_History <- as.factor(data$Credit_History)
data$Gender <- as.factor(data$Gender)
data$Married <- as.factor(data$Married)
data$Dependents <- as.factor(data$Dependents)
data$Self_Employed <- as.factor(data$Self_Employed)
data$Loan_Amount_Term <- as.factor(data$Loan_Amount_Term)
```

## Remaining Gender & Dependents Imputation 
```{r, message=FALSE, warning=FALSE}
#To predict Remaining Gender by (Mode Imputation) & Dependents
levels(data$Gender)[levels(data$Gender)==""] <- "Male"
levels(data$Dependents)[levels(data$Dependents)==""] <- "0"
```

## K-Nearest Neighbours for missing Loan Amount Imputation
```{r, message=FALSE, warning=FALSE}
preProcValues <- preProcess(data, method = c("knnImpute","center","scale"))
Complete_Data_processed <- predict(preProcValues, data)
```

# Data Preprocessing

## Checking Coorelation
```{r, message=FALSE, warning=FALSE}
#Checking for High Coorelation among Numeric variables
cor(Complete_Data_processed[,c(7,8,9)])
#No strong coorelation, So moving ahead. 
```

## Splitting Training and Test Data set  
```{r, message=FALSE, warning=FALSE}
#Spliting training set into two parts based on outcome: 70% and 30%
set.seed(1)
index <- createDataPartition(Complete_Data_processed$Loan_Status, p=0.70, list=FALSE)
trainSet <- Complete_Data_processed[ index,-1]
testSet <- Complete_Data_processed[-index,-1]
```

## Creating Train control (3-fold cross validation)  
```{r, message=FALSE, warning=FALSE}
# Create model with default paramters
fitControl <- trainControl(method = "cv", number = 3, savePredictions = 'final', classProbs = T)
```

# Model Building using Multiple Cores

## Using 4 cores to run in parallel for model building
```{r, message=FALSE, warning=FALSE}
cl <- makeCluster(4)
registerDoParallel(cl)
```

## Random Forest model #79.23% Accuracy
```{r, message=FALSE, warning=FALSE}
set.seed(2)
#Training the random forest model
model_rf<-train(Loan_Status~.,data=trainSet,method='rf',trControl=fitControl,tuneLength=5)
#Predicting using random forest model
testSet$pred_rf<-predict(object = model_rf,testSet[,-13])
#Checking the accuracy of the random forest model
confusionMatrix(testSet$Loan_Status,testSet$pred_rf)
```

## k-Nearest Neighbors (KNN) #74.86% Accuracy  
```{r, message=FALSE, warning=FALSE}
set.seed(3)
#Training the knn model
model_knn<-train(Loan_Status~.,data=trainSet,method='knn',trControl=fitControl,tuneLength=3)
#Predicting using knn model
testSet$pred_knn<-predict(object = model_knn,testSet[,-13])
#Checking the accuracy of the knn model
confusionMatrix(testSet$Loan_Status,testSet$pred_knn)
```

## Logistic Regression #86.34% Accuracy  
```{r, message=FALSE, warning=FALSE}
set.seed(4)
#Training the Logistic regression model
model_lr<-train(Loan_Status~.,data=trainSet,method='glm',trControl=fitControl,tuneLength=3)
#Predicting using Logistic regression model
testSet$pred_lr<-predict(object = model_lr,testSet[,-13])
#Checking the accuracy of the Logistic regression model
confusionMatrix(testSet$Loan_Status,testSet$pred_lr)
```

## SVM (Linear Kernel) #86.34% Accuracy  
```{r, message=FALSE, warning=FALSE}
set.seed(5)
#Training the SVM using Linear Kernel
model_svm<-train(Loan_Status~.,data=trainSet,method='svmLinear',trControl=fitControl,tuneLength=3)
#Predicting using SVM using Linear Kernel
testSet$pred_svm<-predict(object = model_svm,testSet[,-13])
#Checking the accuracy of the svm model
confusionMatrix(testSet$Loan_Status,testSet$pred_svm)
```

## Neural network & Tuning #86.34% Accuracy 
```{r, message=FALSE, warning=FALSE, tidy=TRUE}
#Training the Neural network model
model_nn<- train(Loan_Status~.,data=trainSet,method='nnet',trControl=fitControl,tuneLength=3)
#predict using Neural network model
testSet$pred_nn<-predict(model_nn,testSet[,-13])
#Checking the accuracy of the Neural network model
confusionMatrix(testSet$Loan_Status,testSet$pred_nn)
#tune nueral netwrok #86.34% Accuracy
my.grid <- expand.grid(.decay = c(.5,.1), .size = c(1:3))
model_nn_tune <- train(Loan_Status~.,data=trainSet,method='nnet',trControl=fitControl,tuneLength=3, tuneGrid = my.grid, trace = F)
testSet$pred_nn_tune<-predict(model_nn_tune,testSet[,-13])
#Checking the accuracy of the tuned Neural network model
confusionMatrix(testSet$Loan_Status,testSet$pred_nn_tune)
```

## Stopping Parallel Cluster
```{r, message=FALSE, warning=FALSE, tidy=TRUE}
stopCluster(cl) 
registerDoSEQ()
```

# Conclusion
### Will use Logistic Regression for this dataset as the accuracy is same for Logistic Regression and SVM and Neural network. As time required to train a logistic regression model is less than svm or neural network model.
