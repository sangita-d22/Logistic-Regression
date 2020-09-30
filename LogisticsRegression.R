flierresponse = read.csv("D:/CMS 3rd Sem/ML/FlierResponse.csv", header = T, sep = ",")
flierresponse
str(flierresponse)
flierresponse$Response = as.factor(flierresponse$Response)
summary(flierresponse)
str(flierresponse)

framingham = read.csv("D:/CMS 3rd Sem/ML/framingham.csv")
str(framingham)
framingham$education = as.factor(framingham$education)
str(framingham)

library(caTools)
#carTools: load ten liabries for regression
install.packages("car")
library(car)
#car: contains functions for applied regression, linear models and generalized linear regression
#contains several basic utility functions including: moving(rolling, running) window statistics functions
#DAAG : data analytics and GRaphic data and Functions
#library(DAAG)
#library(rms)
#the rms package offers a variety of tools to build and evaluate regression models in R.
#randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

#split up the data using subset
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

#logistics regression model
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
car:: vif(framinghamLog)
#variance inflation factor, always >/= 1
# the lower aic, the better the model

#accuracy on the training set
predictTrain = predict(framinghamLog, type = "response", newdata = train)
predictTrain

#confusion matrix with thresold of 0.5
table(train$TenYearCHD, predictTrain > 0.5)

#accuracy on train set
(2170+30)/(2170+30+357+9)
#precision= TP/TP+FP
(2170)/(2170+357)
#recall= TP/TP+FN
(2170)/(2170+9)
#specificity = TN/TN+FP
(30)/(30+357)

#predictions on the test set
predictTest = predict(framinghamLog, type = "response", newdata = test)

#confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)

#accuracy on the test set
(915+12)/(915+12+158+7)


#test set AUC
library(ROCR)
library(ggplot2)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
#higher auc, better model
ROCRperf<-performance(ROCRpred,"tpr","fpr")
par(mfrow=c(1,1))
plot(ROCRperf,colorize=TRUE,print.cutoffs.at~seq(0.1,by~0.1),text.adj~c(-0.2,1.7))














