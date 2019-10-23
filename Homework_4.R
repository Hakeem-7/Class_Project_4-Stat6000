# Q5(a)
library(ISLR)
library(MASS)
RNGkind(sample.kind = "Rounding") #To correct the RNG of different R versions 
set.seed(1)
attach(Default)
fit.glm1.0 <- glm(default~income+balance,
               data = Default, family = binomial)
summary(fit.glm1.0)$coef #both predictors are statistically significant

# Q5(b)
## Sample splitting
set.seed(1)
smpl.size <- floor(0.5*nrow(Default))
train <- sample(seq_len(nrow(Default)), size = smpl.size)
def.train <- Default[train, ]
def.test <- Default[-train, ]
def.c<-default[-train]

# dim(Default)
# dim(def.train)
# dim(def.test)

## Fitting Multiple Logistic Regression
fit.glm1.1<-glm(default~income+balance, 
                data = Default, family = binomial, subset = train)
prob.glm<-predict(fit.glm1.1, def.test, type = "response")
pred.glm<-rep("No", 5000)
pred.glm[prob.glm>.5]<-"Yes"
table(pred.glm,def.c)
mean(pred.glm==def.c) #model accuracy is 97.1%
# At the granular level, the confusion matrix suggests that the actual "default" miscalculation
# is approx. 68.9%! such poor precision level may be unacceptable to a credit card company.

## Validation set error
mean(pred.glm!= def.c) #The test (validation) error is 0.0286

# Q5(c)
set.seed(2)
smpl.size <- floor(0.5*nrow(Default))
train <- sample(seq_len(nrow(Default)), size = smpl.size)
def.train <- Default[train, ]
def.test <- Default[-train, ]
def.c<-default[-train]
fit.glm1.1<-glm(default~income+balance, 
                data = Default, family = binomial, subset = train)
prob.glm<-predict(fit.glm1.1, def.test, type = "response")
pred.glm<-rep("No", 5000)
pred.glm[prob.glm>.5]<-"Yes"
table(pred.glm,def.c)
mean(pred.glm!= def.c) #The test (validation) error is 0.0276

set.seed(3)
smpl.size <- floor(0.5*nrow(Default))
train <- sample(seq_len(nrow(Default)), size = smpl.size)
def.train <- Default[train, ]
def.test <- Default[-train, ]
def.c<-default[-train]
fit.glm1.1<-glm(default~income+balance, 
                data = Default, family = binomial, subset = train)
prob.glm<-predict(fit.glm1.1, def.test, type = "response")
pred.glm<-rep("No", 5000)
pred.glm[prob.glm>.5]<-"Yes"
table(pred.glm,def.c)
mean(pred.glm!= def.c) #The test (validation) error is 0.0248

set.seed(4)
smpl.size <- floor(0.5*nrow(Default))
train <- sample(seq_len(nrow(Default)), size = smpl.size)
def.train <- Default[train, ]
def.test <- Default[-train, ]
def.c<-default[-train]
fit.glm1.1<-glm(default~income+balance, 
                data = Default, family = binomial, subset = train)
prob.glm<-predict(fit.glm1.1, def.test, type = "response")
pred.glm<-rep("No", 5000)
pred.glm[prob.glm>.5]<-"Yes"
table(pred.glm,def.c)
mean(pred.glm!= def.c) #The test (validation) error is 0.0262

# The test error was different for the three different sample splits.

# Q5(d)
set.seed(1)
smpl.size <- floor(0.5*nrow(Default))
train <- sample(seq_len(nrow(Default)), size = smpl.size)
def.train <- Default[train, ]
def.test <- Default[-train, ]
def.c<-default[-train]
fit.glm1.2<-glm(default~income+balance+student, 
                data = Default, family = binomial, subset = train)
prob.glm<-predict(fit.glm1.2, def.test, type = "response")
pred.glm <- ifelse(prob.glm > 0.5, "Yes", "No") #smooth right? lol
table(pred.glm,def.c)
mean(pred.glm!= def.c)#The test (validation) error is 0.0288
# Inluding a dummy variable for "student" resulted in an increase in test error by 0.002, which is practically insignificant.
# Hence, it can be deduced that the inclusion of the dummy variable in the logistic regression model is redundant.


# Q6(a) 
set.seed(1)
smpl.size <- floor(0.5*nrow(Default))
train <- sample(seq_len(nrow(Default)), size = smpl.size)
def.train <- Default[train, ]
def.test <- Default[-train, ]
def.c<-default[-train]
fit.glm1.1<-glm(default~income+balance, 
                data = Default, family = binomial, subset = train)
summary(fit.glm1.1)

# Q6(b)
boot.fn <- function(data,index){
  fit.glm1.3 <- glm(default~income+balance, data=data[index, ], family = binomial)
  return(coef(fit.glm1.3))
}
boot.fn(Default, 1:10000)

# Q6(c)
set.seed(1)
boot(Default,boot.fn,1000)

# Q6(d)

# The disparity in the estimated standard errors of the logistic regression and bootstrap, 
# may be attributed to the inadequacy of the fitted model in the logistic regression.
# Further, the bootstrap approach does not assume that the variablility only comes from the irreducible error, as compared to the glm


# Question 8 - Cross-Validation

# Q8(a) - Generate a simulated data set.

set.seed(1)
y<-rnorm(100)
x<-rnorm(100)
y<-x-2*x^2+rnorm(100)
# n = y = 100; p = x = 100
# \begin{align*}
# y = x - 2x^2 + \epsilon\\
# \end{align*}

## Q8(b)

data<-data.frame(y,x)
library(ggplot2)
ggplot(data, aes(x=x, y=y))+ 
  geom_point(color = "darkred", size = 2)+
  ggtitle("Graph of Y vs X")+
  theme(plot.title = element_text(hjust = 0.5))

# The graph obtained looks like an inverse quadratic graph. This suggests that the relationship between x and y is non-linear.

## Q8(c)
set.seed(1)
library(boot)
cv.error=rep(0,4)
for (j in 1:4){
  glm.fit<-glm(y~poly(x, j), data = data)
  cv.error[j]<-cv.glm(data,glm.fit)$delta[1]
}
cv.error

## Q8(d)
set.seed(2)
library(boot)
cv.error=rep(0,4)
for (j in 1:4){
  glm.fit<-glm(y~poly(x, j), data = data)
  cv.error[j]<-cv.glm(data,glm.fit)$delta[1]
}
cv.error
# The LOOCV errors are the same. This is expected since there is no randomness in the tranining/validation data set splits.

## Q8(e)
# The quadratic model had the lowest LOOCV error. 
# This is expected since y is a polynomial of the second order (i.e quadratic) that is dependent on x.

## Q8(f)
set.seed(1)
for (j in 1:4){
  print(summary(glm(y~poly(x, j), data = data)))
}

# The quadratic term in the quadratic, cubic, and quartic model is statistically significant, while the rest are not.
# This agrees with the conclusion drawn from the CV analysis that suggests that the quadratic model outperforms the rest.

# Question 9

## Q9(a)
attach(Boston)

mu<-mean(medv)
mu #mu = 22.53

## Q9(b) - Standard Error
se<-sd(medv)/sqrt(length(medv)) 
se #standard error is 0.409

## Q9(c)
set.seed(1)
boot(medv,function(x,index){mean(x[index])},R<-1000)
# The standard error is almost the same as the estimated standard error in (b)

## Q9(d)
# set.seed(1)
# boot.ci(boot(medv,function(x,index){mean(x[index])},R<-1000), type = "bca")
# boot.ci(boot.out = boot(medv,function(x,index){mean(x[index])},R<-1000), conf = .95)

t.test(Boston$medv)
CI.mu.hat<-c(mu-2*se,mu+2*se)
CI.mu.hat
# The confidence interval obtained using the central limit theorem approach and t-test approach are almost the same.

## Q9(e)
mu.med<-median(medv)
mu.med 

## Q9(f)
set.seed(1)
boot(medv, function(x,index){median(x[index])},1000)
# The standard error of the median is approx. 0.378.

## Q9(g)
mu0.1<-quantile(medv, .1)
mu0.1

## Q9(h)
set.seed(1)
boot(medv, function(x,index){quantile(medv[index],.1)},1000)
#The estimated standard error of the 10th percentile of medv in Boston suburbs is 0.477.

