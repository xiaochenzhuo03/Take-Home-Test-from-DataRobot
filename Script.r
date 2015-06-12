##############################################################
#### Data:Auto.csv 300+row,  9 variable
#### Goal: Classify by value of mpg (bigger or smaller than median) based on other variable
#### Error rate:
####        LDA: 10%
####        QDA: 11%
####        Logistic Regression: 33%
####        K-NN: 9% (k=5) other k has larger error rate
##############################################################

###1.load the csv file into the Bigquery.

###2.Install and load the bigquery package, connect to the Bigquery, query the data, (you may need authentication)
install.packages(bigrquery)
library(bigrquery)
project <- "flash-park-97220"
sql <- "SELECT * FROM [data_xiaochenzhuo.car] "
Auto = query_exec(sql, project = project)

###3.Add a new column mpg01, we will not use mpg even in the training data
mpg01=rep(1,nrow(Auto))
mpg01[Auto$mpg<median(Auto[,1])]=0	
Auto1 =data.frame(Auto ,mpg01)
Auto1 = Auto1[c(2:10)]

###4.Inspect the data graphically, and numerically
set.seed(10)
pairs(Auto1[sapply(Auto1, is.numeric)])
cor(Auto1[sapply(Auto1, is.numeric)])
attach(Auto1)

###5.Split the data as train and test, row with even year num will be in training set.
Train = (year%%2==0)
Test = !Train
Training = Auto1[Train,]
Testing = Auto1[Test,]


###6.LDA
library(MASS)
lda_model<-lda(mpg01~cylinders+displacement+horsepower+weight+year+origin)
summary(lda_model)
lda_predict<-predict(lda_model,Testing)$class
mean(lda_predict!=Testing$mpg01)


###7.QDA
qda_model<-qda(mpg01~cylinders+displacement+horsepower+acceleration+weight+year+origin)
summary(qda_model)
qda_predict<-predict(qda_model,Testing)$class
mean(qda_predict!=Testing$mpg01)

###8.Logistic Regression
logistic_model = glm(mpg01~cylinders+displacement+horsepower+acceleration+weight+year+origin,data = Training,family= binomial)
summary(logistic_model)
logistic_predict_prob = predict(logistic_model,data = Testing, type = "response")
n = nrow(Testing)
logistic_predict_prob = logistic_predict_prob[1:n]
l_mpg01=rep(1,nrow(Testing))
l_mpg01[logistic_predict_prob<0.5]=0
mean(l_mpg01!=Testing$mpg01)


###9.KNN
library(class)
std_data = Auto1[,-8]
std_data = scale(std_data)
Result = std_data[,8]
std_data=std_data[,c(1:7)]
Training = std_data[Train,]
Testing = std_data[Test,]
Training_result = Result[Train]
Testing_result = Result[Test]
knn_result = knn(Training,Testing,Training_result,3)
mean(knn_result!=Testing_result)



                      