############################ SVM Digit Recogniser #################################
# 1. Problem Statement
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the 0-9 numerical digits 

#####################################################################################

# 2. Data Understanding: 
# Number of Instances: 60,000
# Number of Attributes: 735

#3. Data Preparation: 
setwd("/Users/nupur/Upgrad R/predictive analytic 2/SVM/SVM assignment/SVM Dataset")

#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)

#Enable parallel processing for faster computation

install.packages("doParallel")
library(doParallel)
cl <- makePSOCKcluster(3)
registerDoParallel(cl)


#Loading Data

train_data <- read.csv("mnist_test.csv", header = FALSE)
test_data<-read.csv("mnist_train.csv",header = FALSE)

#Understanding Dimensions

dim(train_data)
dim(test_data)

#Structure of the dataset

str(train_data)

#printing first few rows

head(train_data)

#Exploring the data

summary(train_data)
summary(test_data)

#checking missing value

sapply(train_data, function(x) sum(is.na(x)))  #no missing values
sapply(test_data, function(x) sum(is.na(x)))   #no missing values

#Making our target class to factor

train_data$V1<-factor(train_data$V1)
test_data$V1<-factor(test_data$V1)

# Defining test and train data

train = train_data

test = test_data

#Constructing Model

#Using Linear Kernel
Model_linear <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "vanilladot")

Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$V1)

##########################################LINEAR KERNEL SVM MODEL##############################################
#Overall Statistics

#Accuracy : 0.9076        
#95% CI : (0.9053, 0.91)
#No Information Rate : 0.1124        
#P-Value [Acc > NIR] : < 2.2e-16     

#Kappa : 0.8973        
#Mcnemar's Test P-Value : < 2.2e-16     
###############################################################################################################

#Model Using RBF Kernel
Model_RBF <- ksvm(V1~ ., data = train, scaled = FALSE, kernel = "rbfdot")

#Perform prediction on test data
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$V1)

##########################RBF KERNEL SVM MODEL#################################################################
#Support Vector Machine object of class "ksvm" 

#SV type: C-svc  (classification) 
#parameter : cost C = 1 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  1.6237108516675e-07 

#Number of Support Vectors : 3689 

#Overall Statistics

#Accuracy : 0.9533         
#95% CI : (0.9516, 0.955)
#No Information Rate : 0.1124         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.9481         
#Mcnemar's Test P-Value : < 2.2e-16   

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5, verboseIter=TRUE)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
#the values of sigma and c are chosen for tuning taking previous model results as a base
set.seed(7)

grid <- expand.grid(.sigma=c(0.00000016, 0.00000025,0.00000035), .C=c(1,2,3) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(V1~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

fit.svm
plot(fit.svm)

Eval_radial<- predict(fit.svm, test)

#confusion matrix - Radial Kernel
confusionMatrix(Eval_radial,test$V1)

####################################RESULTS OF RADIAL KERNEL SVM MODEL (Tuned hyperparameters)##################################################################################

#10000 samples
#784 predictor
#10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

#No pre-processing
#Resampling: Cross-Validated (5 fold) 
#Summary of sample sizes: 7998, 8001, 7999, 8001, 8001 
#Resampling results across tuning parameters:
  
#  sigma    C  Accuracy   Kappa    
#1.6e-07  1  0.9621001  0.9578720
#1.6e-07  2  0.9665001  0.9627628
#1.6e-07  3  0.9681001  0.9645407
#2.5e-07  1  0.9671004  0.9634298
#2.5e-07  2  0.9703005  0.9669868
#2.5e-07  3  0.9709999  0.9677641
#3.5e-07  1  0.9697004  0.9663202
#3.5e-07  2  0.9718000  0.9686537
#3.5e-07  3  0.9728002  0.9697657

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 3.5e-07 and C = 3.
#Confusion Matrix and Statistics
#Prediction
#   0    1    2    3    4    5    6    7    8    9
#0 5827    2   16    4    7   16   15   17    9   24
#1    4 6624   16   16   10    7   12   26   55    7
#2   16   40 5757   95   21   11   13   38   45   17
#3    2   11   26 5780    0   54    1    5   38   54
#4    8   16   35    2 5684   15   13   48   19   83
#5   12   11    4   99    2 5232   44    4   57   18
#6   23    2   18    7   29   51 5804    3   23    1
#7    1   14   46   33   14    1    0 6043   10   84
#8   25    6   34   72    8   19   16   10 5559   35
#9    5   16    6   23   67   15    0   71   36 5626

#Overall Statistics

#Accuracy : 0.9656         
#95% CI : (0.9641, 0.967)
#No Information Rate : 0.1124         
#P-Value [Acc > NIR] : < 2.2e-16      

#Kappa : 0.9618         
#Mcnemar's Test P-Value : < 2.2e-16      

#Statistics by Class:
  
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity           0.98379   0.9825  0.96626  0.94275  0.97295  0.96514  0.98074   0.9646  0.95009  0.94571
#Specificity           0.99797   0.9971  0.99452  0.99645  0.99559  0.99540  0.99710   0.9962  0.99584  0.99558
#Pos Pred Value        0.98147   0.9774  0.95110  0.96801  0.95965  0.95422  0.97366   0.9675  0.96110  0.95925
#Neg Pred Value        0.99822   0.9978  0.99627  0.99350  0.99708  0.99653  0.99789   0.9959  0.99461  0.99403
#Prevalence            0.09872   0.1124  0.09930  0.10218  0.09737  0.09035  0.09863   0.1044  0.09752  0.09915
#Detection Rate        0.09712   0.1104  0.09595  0.09633  0.09473  0.08720  0.09673   0.1007  0.09265  0.09377
#Detection Prevalence  0.09895   0.1129  0.10088  0.09952  0.09872  0.09138  0.09935   0.1041  0.09640  0.09775
#Balanced Accuracy     0.99088   0.9898  0.98039  0.96960  0.98427  0.98027  0.98892   0.9804  0.97297  0.97064

###########################################################################################################################

#COMPARISION OF LINEAR, RBF AND TUNED RADIAL KERNEL MODEL ACCURACY

#LINEAR- 90.76%
#RBF-    95.3%
#RADIAL TUNED-96.5%

#Thus, Radial Kernel SVM Model is  giving best model for recognizing handwritten numerical letters with 96.5% accuracy
#with tuned hyperparameters at sigma=3.5e-7 and C=3


stopCluster(cl)
