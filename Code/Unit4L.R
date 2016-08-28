library(MASS)
library(ISLR)

#numerical and graphical summary of the data
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-ncol(Smarket)])
#very little correlation between Lagx and Today.
#Volume is increasing over time.

attach(Smarket)
plot(Volume)

#fitting a logistic regression model
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = "binomial")

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

#making predictions
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]

#what are the probabilities for
contrasts(Direction)

#encode the predictions
glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5] <- "Up"

#creating a confusion matrix
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred == Direction)

#creating a training dataset
train = Year<2005
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = "binomial", subset = train)
glm.probs = predict(glm.fit,Smarket.2005, type = "response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs >0.5] <- "Up"

table(glm.pred,Direction.2005)
mean(glm.pred != Direction.2005)

#modifying the model
summary(glm.fit)

glm.fit = glm(Direction~Lag1+Lag2, data = Smarket, family = "binomial", subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")

contrasts(Direction.2005)
glm.pred = rep("Down", 252)
glm.pred[glm.probs>0.5] <- "Up"

table(glm.pred, Direction.2005)
mean(glm.pred != Direction.2005)
106/141

#fitting LDA to the same dataset
lda.fit = lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,2]>0.5)

#fitting a QDA model
qda.fit = qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

#KNN
library(class)
train.x = cbind(Lag1, Lag2)[train,]
test.x = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.x, test.x, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

set.seed(1)
knn.pred = knn(train.x, test.x, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

#Example: Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/nrow(Caravan)

#standardising predictors for KNN to work properly
standardised.x = scale(Caravan[,-86])

var(Caravan[,1])
var(Caravan[,2])

var(standardised.x[,1])
var(standardised.x[,2])

#training and testing data
test = 1:1000
train.x = standardised.x[-test,]
test.x = standardised.x[test,]
train.y = Purchase[-test]
test.y = Purchase[test]

set.seed(1)
knn.pred = knn(train.x, test.x, train.y, k=1)
table(knn.pred, test.y)
mean(knn.pred == test.y)
#Error rate
mean(knn.pred != test.y)
873/(873+50)
mean(test.y != "No")
59/1000

#knn with k=3,5
set.seed(1)
knn.pred2 = knn(train.x, test.x, train.y, k = 3)
table(knn.pred2, test.y)
5/26

set.seed(1)
knn.pred3 = knn(train.x, test.x, train.y, k = 5)
table(knn.pred3, test.y)
4/15

#trying a logistic regression model
glm.fit = glm(Purchase~., data = Caravan, family = "binomial", subset = -test)
glm.prob = predict(glm.fit, Caravan[test,], type = "response")
contrasts(Purchase)

#with a lowered threshold
glm.pred = rep("No",1000)
glm.pred[glm.prob > 0.25] = "Yes" 
table(glm.pred,Purchase[test])
11/33
