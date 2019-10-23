


# Method 1.0 (Extract of the Prof./Textbook approach)
library(ISLR)
RNGkind(sample.kind = "Rounding") #To correct the RNG of different R versions 
attach(Auto)
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)


# Method 1.1

require(caTools)
set.seed(1)
train1 <- sample.split(Auto$name, SplitRatio = .5)
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train1)
mean((mpg-predict(lm.fit,Auto))[!train1]^2)

#Ask the professor this question. (Slight difference in MSE when splitting samples using varying splitting techniques)

# Method 1.2

set.seed(1)
smp.size <- floor(.50*nrow(Auto))
train2 <- sample(seq_len(nrow(Auto)), size = smp.size)
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train2)
mean((mpg-predict(lm.fit,Auto))[-train2]^2)






