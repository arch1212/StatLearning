#ques 10
attach(Weekly)
#a)Numerical and Graphical summaries
dim(Weekly)
names(Weekly)
summary(Weekly)
cor(Weekly[,-ncol(Weekly)])
pairs(Weekly)

#b)logistic regression model
glm.fit1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Weekly, family = "binomial")
summary(glm.fit1)
#Lag2 is significant

#c)
glm.prob1 = predict(glm.fit1, type = "response")
glm.pred1 = rep("Down", 1089)
glm.pred1[glm.prob1 > 0.5] = "Up"
table(glm.pred1, Direction)
mean(glm.pred1 == Direction)

#TPR
557/(557+48)
#92% of the time it correctly predicts if a stock is going up
#FPR
430/(430+54)
#while only coorectly predicting 11% of time if it is going down
# we need to reduce this error for this model to perform better

#we are incorrectly identifying "Down" as "Up" way too much
#if we increase the thresghold the model may perform better by identifying 
#more Downs correctly.  

#error in the baseline model
(557+48)/1089
#i.e. if we predict Up everytime, then the error rate is smaller than the model. 

#d) creating training data for new logistic model
train = Year<2009
Weekly.train = Weekly[train,]
Weekly.test = Weekly[!train,]
Direction.test = Direction[!train]

#fitting logistic regression model
glm.fit2 = glm(Direction~Lag2, 
               data = Weekly, 
               family = "binomial", 
               subset = train)
glm.prob2 = predict(glm.fit2, Weekly.test, type = "response")
glm.pred2 = rep("Down", 104)
glm.pred2[glm.prob2 > 0.5] = "Up"
table(glm.pred2, Direction.test)
mean(glm.pred2 == Direction.test)

#e) using LDA
lda.fit = lda(Direction~Lag2, 
              data = Weekly, 
              subset = train)
lda.fit

lda.class = predict(lda.fit, Weekly.test)$class
table(lda.class, Direction.test)
mean(lda.class == Direction.test)

#f) using QDA
qda.fit = qda(Direction~Lag2, data = Weekly, subset = train)
qda.fit

qda.class = predict(qda.fit, Weekly.test)$class
table(qda.class, Direction.test)
mean(qda.class == Direction.test)

#g) using KNN k=1
train.x = Weekly.train[,c("Lag2"), drop = FALSE]
test.x = Weekly.test[,c("Lag2"),drop = FALSE]
train.y = Direction[train]

set.seed(1)
knn.pred = knn(train.x, test.x, train.y, k=1)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

#h)both lda and logistic regression with an accuracy of 62.5% seems to do a good job

#i)
#different versions of KNN classifier

#k = 3
set.seed(1)
knn.pred = knn(train.x, test.x, train.y, k=3)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)
#increased accuracy

#k=10
set.seed(1)
knn.pred = knn(train.x, test.x, train.y, k=10)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

#k=100
set.seed(1)
knn.pred = knn(train.x, test.x, train.y, k=100)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

#different version of logistic regression model
#including an interaction term
glm.fit3 = glm(Direction~Lag1:Lag2, 
               data = Weekly, 
               family = "binomial", 
               subset = train)
summary(glm.fit3)
glm.prob3 = predict(glm.fit3, Weekly.test, type = "response")
glm.pred3 = rep("Down", 104)
glm.pred3[glm.prob3 > 0.5] = "Up"
table(glm.pred3, Direction.test)
mean(glm.pred3 == Direction.test)

#different version of lda
lda.fit2 = lda(Direction ~ log(abs(Lag2)) + poly(Lag2,2), 
              data = Weekly, 
              subset = train)
lda.fit2

lda.class2 = predict(lda.fit2, Weekly.test)$class
table(lda.class2, Direction.test)
mean(lda.class2 == Direction.test)

#different version of qda
qda.fit = qda(Direction~poly(Lag2,2), data = Weekly, subset = train)
qda.fit

qda.class = predict(qda.fit, Weekly.test)$class
table(qda.class, Direction.test)
mean(qda.class == Direction.test)

detach(Weekly)

#ques 11

attach(Auto)
Auto$mpg01 = rep(0,nrow(Auto))
Auto$mpg01[mpg > median(mpg)] = 1
Auto$mpg01 = as.factor(mpg01)
summary(Auto)

cor(Auto[,c(1:8,10)])
pairs(Auto)

boxplot(mpg~mpg01)
boxplot(cylinders~mpg01)
boxplot(displacement~mpg01)
boxplot(horsepower~mpg01)
boxplot(weight~mgp01)
boxplot(Auto$acceleration~Auto$mpg01)

#mpg,cylinders, displacement, horsepower and weuight seems useful in predicting
#mpg01.

#c) splitting the data into training and testing set using sample.split
library(caTools)
set.seed(88)
split = sample.split(mpg01,SplitRatio = 0.75)
table(split)
train.mpg = subset(Auto, split == TRUE)
test.mpg = subset(Auto, split == FALSE)

#d)LDA
lda.mpg = lda(mpg01~cylinders+displacement+horsepower+weight, data = train.mpg)
lda.mpg.class = predict(lda.mpg, test.mpg)$class
table(lda.mpg.class, test.mpg[,c("mpg01")])
mean(lda.mpg.class == test.mpg[,c("mpg01")])
#16%

#e)QDA
qda.mpg = qda(mpg01~cylinders+displacement+horsepower+weight, data = train.mpg)
qda.mpg.class = predict(qda.mpg, test.mpg)$class
table(qda.mpg.class, test.mpg[,c("mpg01")])
mean(qda.mpg.class == test.mpg[,c("mpg01")])
#15%

#f) logistic regression
glm.mpg = glm(mpg01~cylinders+displacement+horsepower+weight, data = train.mpg, family = "binomial")
summary(glm.mpg)

#removing the insignificant predictors
glm.mpg2 = glm(mpg01~displacement+horsepower+weight, data = train.mpg, family = "binomial")
summary(glm.mpg2)
glm.prob = predict(glm.mpg2, test.mpg, type = "response")
table(glm.prob > 0.5, test.mpg[,c("mpg01")])
14/nrow(test.mpg)
#14%

#g)KNN

# k =1
train.x = train.mpg[,c(2:5)]
test.x = test.mpg[,c(2:5)]
train.y = train.mpg[,c("mpg01")]

set.seed(1)
knn.mpg = knn(train.x, test.x, train.y, k = 1)
table(knn.mpg, test.mpg[,c("mpg01")])
1 -mean(knn.mpg == test.mpg[,c("mpg01")])
#0.1326531 #lowest

#k=3
set.seed(1)
knn.mpg = knn(train.x, test.x, train.y, k = 3)
table(knn.mpg, test.mpg[,c("mpg01")])
1 -mean(knn.mpg == test.mpg[,c("mpg01")])
#0.1734694

#k=5
set.seed(1)
knn.mpg = knn(train.x, test.x, train.y, k = 5)
table(knn.mpg, test.mpg[,c("mpg01")])
1 -mean(knn.mpg == test.mpg[,c("mpg01")])

#k=100
set.seed(1)
knn.mpg = knn(train.x, test.x, train.y, k = 100)
table(knn.mpg, test.mpg[,c("mpg01")])
1 -mean(knn.mpg == test.mpg[,c("mpg01")])

#ques 12 writing functions
#a) 
twotothree <- function(){2^3}
twotothree()

#b)
Power2 <- function(x,a){x^a}
Power2(2,3)

#d) 
Power3 <- function(x,a){
  result <- x^a
  return(result)
}
#e)
x <- 1:10
plot(x,Power3(x,2), 
     xlab = "x", ylab = "x^2", main = "x-square", log ="y")

#f)

plotPower <- function(x,a){
  y <- x^a
  plot(x,x^a)
}

#ques 13

#exploring the data
attach(Boston)
str(Boston)

#creating a new categorical variable
Boston$crim01 = ifelse(Boston$crim > median(Boston$crim),1,0)
attach(Boston)

summary(Boston)
cor(Boston)
pairs(Boston)

#creating training and test set
split = sample.split(crim01, SplitRatio = 0.75)
train.b = subset(Boston, split == TRUE)
test.b = subset(Boston, split == FALSE)

#using logistic regression
glm.b = glm(crim01~nox+age+dis+rad+tax+indus+lstat+zn+black+medv, data = train.b, family = "binomial")
summary(glm.b)

#modifying it
glm.b = update(glm.b, ~.-indus);summary(glm.b)
glm.b = update(glm.b, ~.-lstat);summary(glm.b)

glm.prob = predict(glm.b, test.b, type = "response")
table(glm.prob>0.5, test.b[,c("crim01")])
1 - 114/nrow(test.b)
#9.5% error

#using LDA
lda.b = lda(crim01~nox+age+dis+rad+tax+zn+black+medv, data = train.b)
lda.class = predict(lda.b, test.b)$class
table(lda.class, test.b[,c("crim01")])
1 - 109/nrow(test.b)
#13.4% error

#using QDA
qda.b = qda(crim01~nox+age+dis+rad+tax+zn+black+medv, data = train.b)
qda.class = predict(qda.b, test.b)$class
table(qda.class, test.b[,c("crim01")])
1 - 105/nrow(test.b)
#16.6% error

#using KNN

#k = 1
train.x = train.b[,c(2,5,7,8,9,10,12,14)]
test.x = test.b[,c(2,5,7,8,9,10,12,14)]
train.y = train.b[,c("crim01")]

knn.b = knn(train.x, test.x, train.y, k = 1)
table(knn.b, test.b[,c("crim01")])
1 - mean(knn.b == test.b[,c("crim01")])
#6.3%

knn.b = knn(train.x, test.x, train.y, k = 50)
table(knn.b, test.b[,c("crim01")])
1 - mean(knn.b == test.b[,c("crim01")])
#15%