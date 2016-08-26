#ques 8 
#a)
attach(Auto)
fit1 = lm(mpg~horsepower);summary(fit1)

#i)yes, small p-value of the t-statistic (null hypothesis: coefficient of horsepower = 0)
# and since the p-value is quite small, we can reject the null hypothesis. 
#ii) 
summary(fit1)$sigma/mean(mpg, na.rm = TRUE)
summary(fit1)$r.sq
#iii) negative
#iv) 
predict(fit1, data.frame(horsepower = 98), interval = "confidence")
predict(fit1, data.frame(horsepower = 98), interval = "prediction")

#b)
plot(horsepower, mpg, pch = 20, ylab="MPG", xlab = "Horsepower")
abline(fit1, col = "red", lwd = 4)

#c)
par(mfrow=c(2,2))
plot(fit1)
#U-shaped residuals curve gives evidence of non-linearity.
plot(predict(fit1), rstandard(fit1))
#no evidence of outliers, all of the points have standardised residuals less than 3. 
plot(hatvalues(fit1))

#ques 9
#a)
pairs(Auto)
#b)
cor(Auto[,-ncol(Auto)])
#c)
fit2.auto = lm(mpg~.-name, data = Auto);summary(fit2.auto)
  #i)Yes, since p-value for the f-statistic is very small, we can reject the null hypothesis of 
  #every coefficient being zero.
  #ii)displacement, weight, year, origin
  #iii)for newer models, the milage is better. 
#d)
par(mfrow=c(2,2))
plot(fit2.auto)
#residuals plot suggest non-linearity
par(mfrow = c(1,2))
plot(predict(fit2.auto), rstudent(fit2.auto))
plot(hatvalues(fit2.auto))

#yes there are a few outliers, the points with standardised residuals larger than 3
#high leverage points
which.max(hatvalues(fit2.auto))
#the observation with index 14 has unusually high leverage. 

#e)
fit3.auto = lm(mpg~cylinders*displacement + displacement*weight+ cylinders*weight, data = Auto);
summary(fit3.auto)

#f)
fit4.auto = lm(mpg~log(weight) + sqrt(horsepower) + acceleration + I(acceleration^2), data = Auto)
summary(fit4.auto)
#the predictors are statistically significant

par(mfrow = c(2,2))
plot(fit4.auto)
#the residual plots have a less discernible pattern than in linear regression
#residuals vs. fitted value plot displays heteroscedasticity
#the Q-Q plot deviates from the diagonal, which shows unnormality
par(mfrow = c(1,2))
plot(predict(fit4.auto), rstudent(fit4.auto))
plot(hatvalues(fit4.auto))
#outliers and high leverage points. 

#from all this, we can say that we need a better transformation
par(mfrow=c(1,1))
#using log transformation
fit5.auto = lm(log(mpg)~.-name, data = Auto);summary(fit5.auto)
#R squared
summary(fit4.auto)$r.sq
summary(fit5.auto)$r.sq

par(mfrow = c(2,2))
plot(fit5.auto)
par(mfrow = c(1,2))
plot(predict(fit5.auto), rstudent(fit5.auto))
plot(hatvalues(fit5.auto))
which.max(hatvalues(fit5.auto))
#14
#this model is better than the linear model

#ques 10:
#a)
fit.cs = lm(Sales~Price+Urban+US)
#b)
summary(fit.cs)
contrasts(Urban)
contrasts(US)

#for a unit increase in price, the sales reduces by 54. 
#if the store is in urban location, the sales reduces by 22 units
#if the store is in th US, then the sales increases by 1200 units. 

#d)Price and USYes
#e)
fit2.cs = update(fit.cs, ~.-Urban)
summary(fit2.cs)
#f)
summary(fit.cs)$sigma/mean(Sales, na.rm = TRUE)
summary(fit.cs)$r.sq
summary(fit2.cs)$sigma/mean(Sales, na.rm = TRUE)
summary(fit2.cs)$r.sq
#There is a very little differce in the models based upon how well they fit. 

#g) 
confint(fit2.cs)
#h) outliers and leverage points
par(mfrow=c(1,2))
plot(predict(fit2.cs), rstudent(fit2.cs))
plot(hatvalues(fit2.cs))
#no evidence of outliers
#There may be high leverage observations
3/nrow(Carseats)

# ques 11)
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
#a) regression without an intercept 
fit1.r = lm(y~x+0)
summary(fit1.r)
#the coefficient is significantly different from 0 since t statistic is large and the p-value 
#is small. 

#b)
fit2.r = lm(x~y+0)
summary(fit2.r)
#the coefficient is smaller than x's but it is still significantly different from 0 with high t-statistic
# and a very small p-value to reject the null hypothesis and making this coefficient 
#statistically significant

#c) they are inverse of each other

#d) 
#e)
#f)
fit3.r = lm(y~x);summary(fit3.r)
fit4.r = lm(x~y);summary(fit4.r)

#ques 13
set.seed(1)
#a)
x <- rnorm(100)
#b)
eps <- rnorm(100, mean = 0, sd = 0.5)
#c)
y <- -1 +0.5*x + eps
length(y)
#-1 and 0.5
#d)
plot(y~x)
#there appears to be a moderate linear relationship
#e)
lm.fit1 = lm(y~x) ; summary(lm.fit1)
#x is statistically significant as expected using the p-value of the F-statistic
summary(lm.fit1)$sigma/mean(y)
summary(lm.fit1)$r.sq
#the values are not good as the relationship  is not very strong
# the estimates are very close to real values

#f)
abline(lm.fit1, col = "red")
abline(-1,0.5)
legend()

#g)
lm.fit2 = lm(y~poly(x,2)); summary(lm.fit2)
#no, it doesn't. it is not statistically significant.

#h)
eps = rnorm(100,0,0.1)
y = -1 +0.5*x +eps
plot(y~x)
#the relationship is much more stronger
lm.fit3 = lm(y~x)
summary(lm.fit3)
#the R squared value is much higher because of the strong relationship and the 
#coefficients are much closer to the real coefficients. 
abline(lm.fit3, col = "red", lwd = 4)
abline(-1,0.5 )

#i)
eps = rnorm(100, 0, 1)
y = -1 +0.5*x +eps
plot(y~x)
#the relationship is much weaker
lm.fit4 = lm(y~x)
abline(lm.fit4, col = "red")
summary(lm.fit4)
#the R squared value is very small because of the weak relationship
abline(-1,0.5, lwd = 2)

#j)
confint(lm.fit1)
confint(lm.fit3)
confint(lm.fit4)

#the confidence interval squeezes as the data becomes less noisy with the same centre. 

# ques 14
#a)
set.seed(1)
x1 = runif(100)
x2=0.5*x1 + rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)

#coefficients: 2,0.3

#b)
cor(x1,x2)
plot(x1,x2)
#c)
fitr = lm(y~x1+x2);summary(fitr)
#only x1 is statistically significant
#as per the p-values, there is no relationship between x2 and y
fitr$coefficients
#the estimates are not close to the true coefficients. 

#d)
fit2r = lm(y~x1);summary(fit2r)
#x1 is significantly different from 0, so we can reject the null hypothesis. 

#e)
fit3r =lm(y~x2);summary(fit3r)
#x2 is also statistically different from 0, so we can reject the null. 

#f)
#yes, they do, because of the collinearity between the x1 and x2. it becomes hard to distinguish
#who is responsible for what effect in response y
#so, the entire affect is explained by one variable which makes the other one redundant. 
#this can also be seen from the definition of the coefficients in multiple regresssion
#

#g)
x1 = c(x1,0.1)
x2= c(x2,0.8)
y = c(y,6)

fit1 = lm(y~x1+x2);summary(fit1)
fit2 = lm(y~x1);summary(fit2)
fit3 = lm(y~x2);summary(fit3)

#effect: now the x2 becomes significant the multiple regression model

#ques 15
#a)
str(Boston)
?Boston

pairs(Boston)

fit.zn = lm(crim~zn);summary(fit.zn)
plot(crim~zn)
#significant

fit.indus = lm(crim~indus);summary(fit.indus)
plot(crim~indus)
#significant

Boston$chas = as.factor(Boston$chas)
fit.chas = lm(crim~chas, data = Boston);summary(fit.chas)
plot(crim~chas)
contrasts(Boston$chas)
#not significant

fit.nox = lm(crim~nox);summary(fit.nox)
plot(crim~nox)
#relationship appears to be moderately linear
#significant

fit.rm = lm(crim~rm);summary(fit.rm)
plot(crim~rm)
#significant

fit.age = lm(crim~age);summary(fit.age)
plot(crim~age)
#significant

fit.dis = lm(crim~dis);summary(fit.dis)
plot(crim~dis)
#significant

fit.rad = lm(crim~rad);summary(fit.rad)
plot(crim~rad)
#significant

fit.tax = lm(crim~tax);summary(fit.tax)
plot(crim~tax)
#significant

fit.ptratio = lm(crim~ptratio);summary(fit.ptratio)
plot(crim~ptratio)
#significant

fit.black = lm(crim~black);summary(fit.black)
plot(crim~black)
#significant

fit.lstat = lm(crim~lstat);summary(fit.lstat)
plot(crim~lstat)
#significant

fit.medv = lm(crim~medv);summary(fit.medv)
plot(crim~medv)
#significant

#b)
fit.all = lm(crim~., data = Boston);summary(fit.all)
#only few of the predictors are statistically significant.
#correlation between different predictors may be the cause
#significant: zn, nox,dis,rad,black,lstat,medv

#c)
x = c(fit.zn$coefficients[2],fit.indus$coefficients[2],fit.chas$coefficients[2],fit.nox$coefficients[2],
fit.rm$coefficients[2],fit.age$coefficients[2],fit.dis$coefficients[2],fit.rad$coefficients[2],
fit.tax$coefficients[2],fit.ptratio$coefficients[2],fit.black$coefficients[2],fit.lstat$coefficients[2],
fit.medv$coefficients[2])

plot(x,fit.all$coefficients[-1])
which.max(fit.all$coefficients[-1])
