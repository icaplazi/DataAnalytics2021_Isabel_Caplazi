##### SVM function #####
library(ggplot2)
library(e1071)

set.seed (1)
# creation of two classes
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
x
y

# plot of classes -> helps decide if they can be linearly split
plot(x, col=(3-y))

# create data frame with the response coded as a factor.
dat <- data.frame(x = x,y  = as.factor(y)) # creates data frame of x and y
svmfit <- svm(y ~., data=dat, kernel="linear", cost=10, scale=FALSE)

# The argument scale = FALSE dictates that each features does not have mean zero or stdev one;
plot(svmfit , dat) # maroon = 1; light yellow = -1

# x's on plot designate the indexed values 
svmfit$index # 1,2,5,7,14,16,17

help("svm")
summary(svmfit) # cost = 10; gamma = 0.5; 7 support vectors


# lowering cost, increases margin *more violations
# cost value of 0.1
svmfit2 <- svm(y~., dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit2, dat)

# x's on plot designate the indexed values 
svmfit$index # 1,2,3,4,5,7,9,10,12,13,14,15,16,17,18,20


### tune function ###
set.seed(1)
# varies cost and compares them
tune.out <- tune(svm, y~., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod = tune.out$best.model # gives best of options calculated by tune.out
summary(bestmod) # best model has lowest error


### use predict function ###
# generate test dataset
xtest = matrix(rnorm(20*2), ncol =2)
ytest = sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1]=xtest[ytest==1] + 1
testdat = data.frame(x=xtest, y=as.factor(ytest))

# uses best model to predict class values
ypred <- predict(bestmod, testdat) # uses best model to predict
table(predict = ypred, truth = testdat$y) # compares predicted data to real data
# 19 points correctly classified

# use cost = 0.01 as predict function instead
svmfit3 <- svm(y~., data=dat, kernel = "linear", cast = 0.01, scale = FALSE)
ypred = predict(svmfit3, testdat)
table(predict=ypred, truth=testdat$y) # 18 correctly classified, more unclassified than 0.1


### Linearly separable example ###
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat=data.frame(x=x,y=as.factor(y))
svmfit4 <-svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit4)
plot(svmfit4,dat) # only 3 misclassifications
# small margins overall, o values close to line

svmfit5 <- svm(y~., data=dat, kernel="linear", cost = 1)
summary(svmfit5)
plot(svmfit5, dat) # better fit than the cost = 1e5 because of the larger margin,
# 7 support vectors, and only one misclassification



##### SVM function: Gene Expression Dataset #####
library(ISLR)
names(Khan) # data set consists of expression measurements for 2,308 genes.
dim(Khan$xtrain) # 63 observations
dim(Khan$xtest) # 20 observations

# gives lengths and tables train and test datasets
length(Khan$ytrain )
length(Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

# predict cancer subtype using gene expression measurements
# use linear kernel because of the large data set; don't need variability of radial or polynomial
# training data set
dat <- data.frame(x=Khan$xtrain , y = as.factor(Khan$ytrain ))
svm.Khan <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(svm.Khan) # 58 support vectors
# no training errors because there is easy separation between each observation

# test data set
dat.te=data.frame(x=Khan$xtest , y = as.factor(Khan$ytest ))
pred.te=predict(svm.Khan, newdata=dat.te)
table(pred.te, dat.te$y)
# 2 set errors
# errors are found on 2 vs. 3 in table (diagonal is correctly classified)


##### SVM function: Group 3 (svm #1) #####
n <- 150 # number of data points
p <- 2 # dimension
sigma <- 1 # variance of the distribution
meanpos <- 0 # center of the distribution of positive examples
meanneg <- 3 # center of the distribution of negative examples
npos <- round(n/2) # number of positive examples
nneg <- n-npos # number of negative examples

# Generate the positive and negative examples
xpos <- matrix(rnorm(npos*p,mean=meanpos,sd=sigma),npos,p)
xneg <- matrix(rnorm(nneg*p,mean=meanneg,sd=sigma),npos,p)
x <- rbind(xpos,xneg)

# Generate the labels
y <- matrix(c(rep(1,npos),rep(-1,nneg)))

# Visualize the data
plot(x,col=ifelse(y>0,1,2))
legend("topleft",c('Positive','Negative'),col=seq(2),pch=1,text.col=seq(2))

ntrain <- round(n*0.8) # number of training examples
tindex <- sample(n,ntrain) # indices of training samples
xtrain <- x[tindex,]
xtest <- x[-tindex,]
ytrain <- y[tindex]
ytest <- y[-tindex]
istrain=rep(0,n)
istrain[tindex]=1

# Visualize
plot(x,col=ifelse(y>0,1,2),pch=ifelse(istrain==1,1,2))
legend("topleft",c('Positive Train','Positive Test','Negative Train','Negative Test'),col=c(1,1,2,2), pch=c(1,2,1,2), text.col=c(1,1,2,2))

# SVM fit for training data
dat <- data.frame(xtrain , y = as.factor(ytrain))
svm.out1 <- svm(y ~., data=dat, kernel="linear",cost=10)
summary(svm.out1) # 5 support vectors
plot(svm.out1 , dat) # maroon = 1; light yellow = -1; 5 misclassified

# test data set
dat.test=data.frame(xtest , y = as.factor(ytest))
pred.test=predict(svm.out1, newdata=dat.test)
table(pred.test, dat.test$y)# The argument scale = FALSE dictates that each features does not have mean zero or stdev one;
plot(svm.out1 , dat.test) # maroon = 1; light yellow = -1; 5 misclassified
