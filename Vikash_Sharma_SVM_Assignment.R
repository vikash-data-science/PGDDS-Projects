############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
# 4.1 Linear kernel
# 4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation
# 6 Key Observations and Insights

#####################################################################################
# 1. Business Understanding: 
# The objective is to correctly classify the handwritten digits based on the pixel values given as features.
#####################################################################################

# 2. Data Understanding: 
# MNIST data which is a large database of handwritten digits where we have pixel values of each digit along with its label.
# Number of Attributes: 785 
# Number of Instances in Train dataset: 60,000
# Number of Instances in Test dataset: 10,000

# 3. Data Preparation: 

# Loading Necessary libraries
library(kernlab)
library(readr)
library(caret)
library(caTools)
library(e1071)

# Loading Data
Data_tr <- read.csv("mnist_train.csv", header=FALSE)
Data_tst <- read.csv("mnist_test.csv", header=FALSE)

# Understanding Dimensions
dim(Data_tr)
# 60000   785
dim(Data_tst)
# 10000   785

# Structure of the dataset
str(Data_tr)
str(Data_tst)

# Printing first few rows
head(Data_tr)
head(Data_tst)

# Exploring the data
summary(Data_tr)
summary(Data_tst)

# Checking missing values
sapply(Data_tr, function(x) sum(is.na(x)))
sum(is.na(Data_tr))
# 0                              # no missing values in training dataset
sapply(Data_tst, function(x) sum(is.na(x)))
sum(is.na(Data_tst))
# 0                             # no missing values in test dataset

# Making our target class (digit) to factor
Data_tr$V1 <-factor(Data_tr$V1)
Data_tst$V1 <-factor(Data_tst$V1)

# Split the data into train and test set. Since MNIST dataset has large size and SVM is computationally intensive, we take a sample of 5% from the respective training and test datasets for our analysis.
set.seed(100)
train.indices = sample.split(Data_tr$V1, SplitRatio = 0.05)
test.indices = sample.split(Data_tst$V1, SplitRatio = 0.05)
train = Data_tr[train.indices, ]
test = Data_tst[test.indices, ]

# Renaming target variable appropriately
colnames(train)[1] <- c("digit")
colnames(test)[1] <- c("digit")

######################################################################
# 4. Model Building
#---------------------------------------------------------------------
# 4.1 Linear - Kernel
######################################################################

# Using Linear Kernel
Model_linear <- ksvm(digit ~ ., data = train, scale = FALSE, kernel = "vanilladot")
Model_linear
##########################################################################################
# Support Vector Machine object of class "ksvm" 
#
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
#
# Linear (vanilla) kernel function. 
#
# Number of Support Vectors : 1233 
#
# Objective Function Value : 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1e-04 0 0 0 0 0 0 0 -1e-04 0 0 -1e-04 0 0 0 0 0 -1e-04 0 0 -1e-04 0 0 0 0 0 -2e-04 0 
# Training error : 0 
##########################################################################################

# Predicting the model results
Eval_linear <- predict(Model_linear, test)

# Testing and Confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)
##########################################################################################
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  0  1  2  3  4  5  6  7  8  9
#          0 47  0  3  0  1  0  2  0  0  1
#          1  0 55  0  1  0  0  0  1  2  1
#          2  0  1 46  1  0  0  1  1  1  0
#          3  0  0  1 46  0  5  0  0  1  1
#          4  1  0  0  0 44  0  0  2  1  4
#          5  0  0  0  1  0 37  3  0  2  0
#          6  1  1  1  0  0  0 42  0  0  0
#          7  0  0  1  0  0  1  0 45  0  0
#          8  0  0  0  1  0  2  0  1 42  0
#          9  0  0  0  0  4  0  0  1  0 43
#
# Overall Statistics
#                                           
#                Accuracy : 0.894           
#                  95% CI : (0.8636, 0.9196)
#     No Information Rate : 0.114           
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.8822          
#  Mcnemar's Test P-Value : NA              
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            0.9592   0.9649   0.8846   0.9200   0.8980   0.8222   0.8750
# Specificity            0.9845   0.9887   0.9888   0.9822   0.9823   0.9868   0.9934
# Pos Pred Value         0.8704   0.9167   0.9020   0.8519   0.8462   0.8605   0.9333
# Neg Pred Value         0.9955   0.9955   0.9866   0.9910   0.9888   0.9825   0.9868
# Prevalence             0.0980   0.1140   0.1040   0.1000   0.0980   0.0900   0.0960
# Detection Rate         0.0940   0.1100   0.0920   0.0920   0.0880   0.0740   0.0840
# Detection Prevalence   0.1080   0.1200   0.1020   0.1080   0.1040   0.0860   0.0900
# Balanced Accuracy      0.9718   0.9768   0.9367   0.9511   0.9401   0.9045   0.9342
#                      Class: 7 Class: 8 Class: 9
# Sensitivity            0.8824   0.8571   0.8600
# Specificity            0.9955   0.9911   0.9889
# Pos Pred Value         0.9574   0.9130   0.8958
# Neg Pred Value         0.9868   0.9846   0.9845
# Prevalence             0.1020   0.0980   0.1000
# Detection Rate         0.0900   0.0840   0.0860
# Detection Prevalence   0.0940   0.0920   0.0960
# Balanced Accuracy      0.9389   0.9241   0.9244
##########################################################################################

########################################################################################################################################
# Hyperparameter tuning and Cross Validation - Linear - SVM
########################################################################################################################################

# We will use the train function from caret package to perform Cross Validation. 

# traincontrol function - Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.
trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

# Expand.grid functions takes set of hyperparameters (C values), that we shall pass to our model.
set.seed(1)
grid <- expand.grid(C=seq(1, 9, by=2))

# train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method,
# Performing 5-fold cross validation
fit.svm <- train(digit ~ ., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm)
##########################################################################################
# Support Vector Machines with Linear Kernel 
#
# 3000 samples
#  784 predictor
#   10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 2401, 2400, 2401, 2398, 2400 
# Resampling results across tuning parameters:
#
#   C  Accuracy   Kappa      Accuracy SD  Kappa SD  
#   1  0.8993341  0.8880966  0.01127883   0.01254275
#   3  0.8993341  0.8880966  0.01127883   0.01254275
#   5  0.8993341  0.8880966  0.01127883   0.01254275
#   7  0.8993341  0.8880966  0.01127883   0.01254275
#   9  0.8993341  0.8880966  0.01127883   0.01254275
#
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was C = 1.
##########################################################################################

# Cross-validation shows that the initial model is the optimal choice for Linear SVM.

######################################################################
# 4.2 RBF Kernel
######################################################################

# Using RBF Kernel
Model_RBF <- ksvm(digit ~ ., data = train, scale = FALSE, kernel = "rbfdot")
Model_RBF
##########################################################################################
# Support Vector Machine object of class "ksvm" 
#
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
#
# Gaussian Radial Basis kernel function. 
#  Hyperparameter : sigma =  1.61211211372059e-07 
#
# Number of Support Vectors : 1695 
# 
# Objective Function Value : -14.9397 -44.1834 -41.2372 -29.9131 -54.6685 -46.641 -29.2048 -39.6623 -31.7869 -43.4564 -38.1785 -33.6748 -38.3008 -28.5917 
# -38.6669 -55.7521 -34.7175 -78.9117 -52.5858 -57.6629 -67.8284 -56.1343 -77.4476 -45.7383 -39.7045 -105.2591 -42.1901 -50.9213 -98.4582 -55.631 -57.8466 
# -50.9585 -60.8002 -54.1086 -113.1154 -66.571 -46.386 -95.1962 -62.4546 -28.5021 -47.7924 -33.8743 -52.4282 -106.0799 -67.5956 
# Training error : 0.024333
##########################################################################################

# Predicting the model results
Eval_RBF <- predict(Model_RBF, test)

# Testing and Confusion matrix - Linear Kernel
confusionMatrix(Eval_RBF,test$digit)
########################################################################################## 
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  0  1  2  3  4  5  6  7  8  9
#          0 47  0  1  0  0  0  1  0  0  1
#          1  0 55  0  1  0  0  0  2  0  1
#          2  0  1 47  0  0  0  0  1  0  0
#          3  0  0  1 49  0  1  0  0  1  1
#          4  0  0  2  0 46  0  0  2  1  2
#          5  1  0  0  0  0 40  1  0  0  0
#          6  1  1  0  0  0  1 46  0  0  0
#          7  0  0  1  0  0  0  0 45  0  0
#          8  0  0  0  0  0  2  0  0 47  0
#          9  0  0  0  0  3  1  0  1  0 45
#
# Overall Statistics
#                                          
#                Accuracy : 0.934           
#                  95% CI : (0.9086, 0.9541)
#     No Information Rate : 0.114           
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                           
#                   Kappa : 0.9266          
#  Mcnemar's Test P-Value : NA              
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            0.9592   0.9649   0.9038   0.9800   0.9388   0.8889   0.9583
# Specificity            0.9933   0.9910   0.9955   0.9911   0.9845   0.9956   0.9934
# Pos Pred Value         0.9400   0.9322   0.9592   0.9245   0.8679   0.9524   0.9388
# Neg Pred Value         0.9956   0.9955   0.9889   0.9978   0.9933   0.9891   0.9956
# Prevalence             0.0980   0.1140   0.1040   0.1000   0.0980   0.0900   0.0960
# Detection Rate         0.0940   0.1100   0.0940   0.0980   0.0920   0.0800   0.0920
# Detection Prevalence   0.1000   0.1180   0.0980   0.1060   0.1060   0.0840   0.0980
# Balanced Accuracy      0.9763   0.9779   0.9497   0.9856   0.9616   0.9422   0.9758
#                      Class: 7 Class: 8 Class: 9
# Sensitivity            0.8824   0.9592   0.9000
# Specificity            0.9978   0.9956   0.9889
# Pos Pred Value         0.9783   0.9592   0.9000
# Neg Pred Value         0.9868   0.9956   0.9889
# Prevalence             0.1020   0.0980   0.1000
# Detection Rate         0.0900   0.0940   0.0900
# Detection Prevalence   0.0920   0.0980   0.1000
# Balanced Accuracy      0.9401   0.9774   0.9444
##########################################################################################

########################################################################################################################################
# Hyperparameter tuning and Cross Validation - Non-Linear - SVM (Non-Linear SVM model exhibits higher accuracy than Linear SVM model)
########################################################################################################################################

# We will use the train function from caret package to perform Cross Validation. 

# traincontrol function - Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.
trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

# Expand.grid functions takes set of hyperparameters ("sigma" and C values), that we shall pass to our model.
set.seed(7)
grid <- expand.grid(.sigma=c(1.61e-07, 2e-07, 2.5e-07, 3e-07, 3.5e-07), .C=c(1,3,5,7,9))

# train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method,
# Performing 5-fold cross validation
fit.svm <- train(digit ~ ., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm)
##########################################################################################
# Support Vector Machines with Radial Basis Function Kernel 
#
# 3000 samples
#  784 predictor
#   10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 2399, 2400, 2401, 2401, 2399 
# Resampling results across tuning parameters:
#
#   sigma     C  Accuracy   Kappa      Accuracy SD  Kappa SD   
#   1.61e-07  1  0.9353315  0.9281179  0.004344296  0.004826674
#   1.61e-07  3  0.9466615  0.9407132  0.006288003  0.006987467
#   1.61e-07  5  0.9483299  0.9425687  0.004901691  0.005446183
#   1.61e-07  7  0.9476638  0.9418293  0.005386393  0.005983944
#   1.61e-07  9  0.9469965  0.9410882  0.004963508  0.005514013
#   2.00e-07  1  0.9379999  0.9310851  0.004773587  0.005304290
#   2.00e-07  3  0.9489949  0.9433069  0.007174626  0.007976038
#   2.00e-07  5  0.9503310  0.9447932  0.006833416  0.007595583
#   2.00e-07  7  0.9506654  0.9451650  0.007880847  0.008759730
#   2.00e-07  9  0.9493304  0.9436817  0.007807123  0.008677234
#   2.50e-07  1  0.9423332  0.9359023  0.003247413  0.003609974
#   2.50e-07  3  0.9513293  0.9459024  0.006425850  0.007142440
#   2.50e-07  5  0.9506638  0.9451629  0.007624684  0.008474841
#   2.50e-07  7  0.9506638  0.9451631  0.007624684  0.008474771
#   2.50e-07  9  0.9509977  0.9455341  0.007528343  0.008367685
#   3.00e-07  1  0.9456660  0.9396083  0.002800386  0.003111544
#   3.00e-07  3  0.9536643  0.9484986  0.006075888  0.006754064
#   3.00e-07  5  0.9526649  0.9473881  0.005980883  0.006647844
#   3.00e-07  7  0.9526649  0.9473881  0.005980883  0.006647844
#   3.00e-07  9  0.9526649  0.9473881  0.005980883  0.006647844
#   3.50e-07  1  0.9499982  0.9444234  0.002675386  0.002972608
#   3.50e-07  3  0.9523327  0.9470187  0.004659577  0.005180222
#   3.50e-07  5  0.9513321  0.9459072  0.004639006  0.005156532
#   3.50e-07  7  0.9513321  0.9459072  0.004639006  0.005156532
#   3.50e-07  9  0.9513321  0.9459072  0.004639006  0.005156532
#
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 3e-07 and C = 3.
##########################################################################################

# Plotting model results
plot(fit.svm)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear <- predict(fit.svm, test)
confusionMatrix(evaluate_non_linear, test$digit)
##########################################################################################
# Confusion Matrix and Statistics
#
#           Reference
# Prediction  0  1  2  3  4  5  6  7  8  9
#          0 49  0  1  0  0  0  2  0  0  1
#          1  0 55  0  0  0  0  0  1  0  1
#          2  0  1 49  0  0  0  0  0  0  0
#          3  0  0  1 49  0  2  0  0  0  1
#          4  0  0  0  0 47  0  0  2  1  1
#          5  0  0  0  1  0 40  0  0  0  0
#          6  0  1  0  0  0  0 46  0  0  0
#          7  0  0  1  0  0  0  0 46  0  0
#          8  0  0  0  0  0  2  0  1 48  0
#          9  0  0  0  0  2  1  0  1  0 46
#
# Overall Statistics
#                                          
#                Accuracy : 0.95            
#                  95% CI : (0.9271, 0.9674)
#     No Information Rate : 0.114           
#     P-Value [Acc > NIR] : < 2.2e-16       
#                                          
#                   Kappa : 0.9444          
#  Mcnemar's Test P-Value : NA              
#
# Statistics by Class:
#
#                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
# Sensitivity            1.0000   0.9649   0.9423   0.9800   0.9592   0.8889   0.9583
# Specificity            0.9911   0.9955   0.9978   0.9911   0.9911   0.9978   0.9978
# Pos Pred Value         0.9245   0.9649   0.9800   0.9245   0.9216   0.9756   0.9787
# Neg Pred Value         1.0000   0.9955   0.9933   0.9978   0.9955   0.9891   0.9956
# Prevalence             0.0980   0.1140   0.1040   0.1000   0.0980   0.0900   0.0960
# Detection Rate         0.0980   0.1100   0.0980   0.0980   0.0940   0.0800   0.0920
# Detection Prevalence   0.1060   0.1140   0.1000   0.1060   0.1020   0.0820   0.0940
# Balanced Accuracy      0.9956   0.9802   0.9700   0.9856   0.9752   0.9433   0.9781
#                      Class: 7 Class: 8 Class: 9
# Sensitivity            0.9020   0.9796   0.9200
# Specificity            0.9978   0.9933   0.9911
# Pos Pred Value         0.9787   0.9412   0.9200
# Neg Pred Value         0.9890   0.9978   0.9911
# Prevalence             0.1020   0.0980   0.1000
# Detection Rate         0.0920   0.0960   0.0920
# Detection Prevalence   0.0940   0.1020   0.1000
# Balanced Accuracy      0.9499   0.9865   0.9556 
##########################################################################################

##########################################################################################
# Key Observations and Insights:
# 1. In this multi-class classification problem with 10 classes, Non-Linear SVM model accuracy of 95% is better than Linear SVM model accuracy of 89%.
# 2. Hyper-parameter tuning and cross-validation helped in raising non-linear SVM model accuracy from 93% to 95%.
# 3. Since we do not see any significant difference in the model accuracy of validation and test datasets, the non-linear SVM model with the RBF 
# kernel turns out to a good choice as the optimum prediction model.
# 4. One of the reasons why the the linear kernel doesn't work as well as the RBF kernel is that the dataset is non-linear.
###########################################################################################
