#Claire Harris
#Tumor classificaiton

library(tidyverse)
library(caret)
library(rpart)
library(partykit)

cancer <- read.csv("/Users/mark/Documents/Notre Dame/Intro Data Science/Data_Sets/FNA_cancer")

# Take a quick look, how many samples, variable types.
glimpse(cancer)

#Don't need that X variable, nor the id.
cancer <- select(cancer, -X, -id)

# check for NA values.
sum(is.na(cancer)) 

# Make the diagnosis a factor.
cancer$diagnosis <- as.factor(cancer$diagnosis)
levels(cancer[['diagnosis']])

# Exploratory diagnosis.
summary(cancer)

# Aim is to build models that recognize malignancy. The Exploratory Data Analysis
# can help this process by helping to identify the variables that are related to
# malignancy.
# An obvious approach is to make a boxplot to see how distribution of variables
# changes when the observations are grouped according to whether they are associated to
# malignancy or not.
# Mean values for these variables are obvious candidates to examine.
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=radius_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=texture_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=perimeter_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=area_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=smoothness_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=compactness_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=concavity_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=concave.points_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=symmetry_mean))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=fractal_dimension_mean))
# Of all the mean related variables, fractal_dimension is the one whose distribution 
# shows least difference across the malignant and benign categories.

# Explore worst related variables, compared to diagnosis.
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=radius_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=texture_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=perimeter_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=area_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=smoothness_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=compactness_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=concavity_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=concave.points_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=symmetry_worst))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=fractal_dimension_worst))

# The variables related to the worst cases of variables all show distributions
# that differ when comparing malignancy related observations to benign cases.
# Explore standard error related variables, compared to diagnosis.

ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=radius_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=texture_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=perimeter_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=area_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=smoothness_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=compactness_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=concavity_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=concave.points_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=symmetry_se))
ggplot(data=cancer) + geom_boxplot(aes(x=diagnosis, y=fractal_dimension_se))

# fractal_dimension_se, symmetry_se, smoothness_se, texture_se showed little
# differentiation in distribution between the malignancy related cases and the
# benign cases.

#set seed
set.seed(1842)
#number of rows in the dataset
n <- nrow(cancer)
#Create training and test data set using a 80/20 ratio from full data set
test_index_dt <- sample.int(n, size=round(0.2*n))
cancer.train.dt <- cancer[-test_index_dt,]
cancer.test.dt <- cancer[test_index_dt,]
#check response distribution between test and training data set
table(cancer.train.dt$diagnosis)
table(cancer.test.dt$diagnosis)
#shortcut formula for response and predictor variables
form1 <- as.formula(diagnosis~.)
#Create decision tree from training set
#Build full tree by setting CP to 0.00 using recursive partitioning tree_cancer <- rpart(form1,data=cancer.train.dt, control = c(cp=0.00))
#plot full tree to find lowest CPP
plotcp(tree_cancer)
#Evaluate complexity values of cross validation
printcp(tree_cancer)
#Get a closer look at the tree that was produced #Partykit plot of tree plot(as.party(tree_cancer))
#Overview of the splits in the nodes
#Root Node: 455 subjects with 169 misclassfications with B being the dominant class; tree_cancer
#Calculate predictions for test data
tp1 <- predict(tree_cancer, newdata = cancer.test.dt, type = "class")
#Confusion matrix from test data
confusion_dt <- table(tp1, cancer.test.dt$diagnosis,dnn=c("actual","predicted"))
confusion_dt
#How accurate the model is:
round((sum(diag(confusion_dt))/length(cancer.test.dt$diagnosis) * 100), 5)
#Misclassification in the model:
1-sum(diag(confusion_dt))/sum(confusion_dt)

#Prune tree with best cp
#set seed
set.seed(1842)
#Prune tree with the Best CP with lowest error rate
prune_tree <- prune(tree_cancer, cp = (tree_cancer$cptable[which.min(tree_cancer$cptable[,xerror]))

#Calculate predictions for test data using pruned tree
tp2 <- predict(prune_tree, newdata = cancer.test.dt, type = "class") #plot full tree to find lowest CPP
plotcp(prune_tree)
#Evaluate complexity values of cross validation
printcp(prune_tree)
#Get a closer look at the tree that was produced #Partykit plot of tree plot(as.party(prune_tree))
#Overview of the splits in the nodes
#Root Node: 454 subjects with 169 misclassfications with B being the dominant class; prune_tree
#Evaluate accuracy and outcome
prune_confusion <- table(tp2, cancer.test.dt$diagnosis,dnn=c("actual","predicted"))
prune_confusion
#How accurate the model is:
round((sum(diag(prune_confusion))/length(cancer.test.dt$diagnosis) * 100), 5)
#Misclassification in the model:
1-sum(diag(prune_confusion))/sum(prune_confusion)


#Classification algorithm using Random Forests/Bagging

# there is not a lot of data, so partition into training and sample sets using the caret
# library, to ensure training and test sets can similar distributions of the response.
test_index <- createDataPartition(cancer$diagnosis,p=0.2,list = F)
cancer.train <- cancer[-test_index,]
cancer.test <- cancer[test_index,]
# Check response distribution.
table(cancer.train$diagnosis)
table(cancer.test$diagnosis)
#shortcut formula for response and predictor variables
form1 <- as.formula(diagnosis~.)

# Train a Random Forest to recognize malignancy.
set.seed(1842)
# out of bag validation (could do cross validation if you want,
# but out of bag is applicable to random forest)
cancer.rf.ctrl <- trainControl(method="oob")
# Parameter for random forest is the ntry paramter. We have 30 variables.
# Take the default for ntrees etc st this point. If the model training does
# not give good results, we can alter these and retry.
cancer.rf.train <- train(form1, data = cancer.train, method = "rf", trControl = cancer.rf.
# Extract the best model, and examine it.
cancer.rf<- cancer.rf.train$finalModel
# Examine the result. check the OOB error rate to see if we like it.
cancer.rf
# summarize the importance of variables in the model.
library(randomForest)
# The relative importance of the variables might help identify if any # are particularly useful or significant in finding malignancy. importance(cancer.rf)
# Check the performance of the model on the test data.
cancer.rf.predict <- predict(cancer.rf, newdata = cancer.test)
confusionMatrix(cancer.rf.predict, cancer.test$diagnosis , positive='M')

#Classification algorithm using Kth Nearest Neighbors
#Rescale variables
rescale_x <- function(x){(x-min(x))/(max(x)-min(x))} cancer2 <- cancer
cancer2[2:31] <- rescale_x(cancer2[2:31])
#Test and training data for knn after rescale
test_index.knn <- createDataPartition(cancer2$diagnosis,p=0.2,list = F)
cancer.train.knn <- cancer[-test_index.knn,]
cancer.test.knn <- cancer[test_index.knn,]

#KNN using caret
ctrl <- trainControl(method="repeatedcv",repeats=3)
knnFit <- train(diagnosis~.,data=cancer.train.knn,method="knn",trControl=ctrl)

#Return KNN fit
knnFit

#Predict from KNN
knnPredict <- predict(knnFit,newdata=cancer.test)
#Confusion Matrix to view accuracy
confusionMatrix(knnPredict,cancer.test$diagnosis)
