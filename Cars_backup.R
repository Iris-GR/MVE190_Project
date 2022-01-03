# Mercedes Cars script 

# The cars data set
setwd("C:/R workspace/MVE190/MVE190_Project_new/MVE190_Project")
#------------------------ Libraries -------------------------------------------#
# Temporary file for looking at the cars dataset

# Load libraries
library(corrplot)
library(vioplot)
library(geosphere)
library(dplyr)
library(ISLR)
library(leaps)
library(olsrr)
library(ggplot2)
library(rgl)  
library(scatterplot3d)  
library(arm)
library(dotwhisker)
library(beepr)
library(car)

##### Dataset preparation ######################################################

# Load the cars dataset
cars.0 <- data.frame(read.csv("CarPrice.csv", header = T ))

# Add the numeric/continuous variables to dataset
cont.var.index <- c(1, 2, 10:14, 17, 19:26)
cars <- data.frame(cars.0[, cont.var.index])

attach(cars.0)
#------------------------ Categorical data ------------------------------------#
# In this section, we factorise the categorical variables to be able to do some 
# more extensive analysis later.

# Add the categorical variables as assigned categorical factors and change the 
# reference level to the desired one
# Car name
cars$CarName <- factor(CarName)

# Fuel type
cars$fueltype <- factor(fueltype)
cars$fueltype <- relevel(cars$fueltype, ref = "gas")

# Aspiration
cars$aspiration <- factor(aspiration)

# Number of doors (excluding trunk dooor)
cars$doornumber <- factor(doornumber)

# Type of car body
cars$carbody <- factor(carbody)
cars$carbody <- relevel(cars$carbody, ref = "hatchback")

# Type of wheel drive
cars$drivewheel <- factor(drivewheel)
cars$drivewheel <- relevel(cars$drivewheel , ref ="rwd")

# Engine location
cars$enginelocation <- factor(enginelocation)

# Engine type
cars$enginetype <- factor(enginetype)
cars$enginetype <- relevel(cars$enginetype, ref = "ohc")

# Number of cylinders
cars$cylindernumber <- factor(cylindernumber)
cars$cylindernumber <- relevel(cars$cylindernumber , ref ="four")

# Fuel system
cars$fuelsystem <- factor(fuelsystem)
cars$fuelsystem <- relevel(cars$fuelsystem , ref ="2bbl")

#### Data overview #############################################################
attach(cars)
#------------------------ Loading data ----------------------------------------#
# In this part of the project it is the dataset CarPrice which will be analysed.

#Notable: there are in total 205 cars in the dataset. 
# Minimum price is 5118 dollar.
# Maximum price is 45400 dollar. 


#----------------------- Sorting and removing variables -----------------------#
# Only three cars have enginelocation == rear. 
# Decided to remove these as they did not feel representative.
#cars <- subset(cars, enginelocation == "front")

# Only small amount of cars with aspiration == turbo,
# Decided to focus on cars with standard aspiration:
#cars <- subset(cars, aspiration == "std")

grDevices::windows()
vioplot(compressionratio, main="Violin plot for compression ratio", ylab = "Compression ratio")
# After lookin at the violin plot for compression ratio, I decided to remove
# all cars with ratio over 15.
#cars <- subset(cars, compressionratio <= 15)

# Possibly also only look at gas cars? It seems that all diesel cars have
# already been excluded??
grDevices::windows()
plot(fueltype,price)

# It seems that the engine size might be grouped too?
grDevices::windows()
vioplot(enginesize, main="Violin plot for engine size", ylab = "Compression ratio")

#cars <- subset(cars, enginesize <= 200)



#cars <- subset(cars, fueltype=="gas")


# Suspect that id is not important for price:
#cars <- subset(cars, select = -c(car_ID))

attach(cars)

#------------------------ Analysis of effect ----------------------------------#
# Here, some preliminary analysis of the variables and their effects are done.

grDevices::windows()
# plot(CarName,price)
pairs(~price+CarName+carbody+carheight)

grDevices::windows()
pairs(~price+carlength+enginetype+enginesize)

grDevices::windows()
pairs(~price+boreratio+peakrpm+highwaympg+citympg)

grDevices::windows()
pairs(~price+symboling+doornumber+drivewheel)

grDevices::windows()
pairs(~price+wheelbase+carwidth+curbweight+cylindernumber)

grDevices::windows()
pairs(~price+fuelsystem+stroke+horsepower)

grDevices::windows()
plot(fueltype,price)


# By themselves:
# Negative impact:
# citympg 
# highwaympg


# Positive impact:
# enginesize, horsepower, carheight, 

#-------------------------- Variance Inflation Factor -------------------------#
# Here, the multicollineraty between the continuous variables is analysed 
# This is done through the usage of the variance inflation factor. 
# When variables which seem highly collinear are located, their explanatory 
# powers for the price are compared, and the least relevant of them removed. 

# Trial 1:
vif(lm(
  price ~ highwaympg + citympg + horsepower + enginesize + stroke + compressionratio + carlength + carheight + curbweight + carwidth + wheelbase))
# Highwaympg and citympg seem dependent on each other. 

mod1 <- lm(price~highwaympg) 
summary(mod1) # r2adj = 0.4841

mod2 <- lm(price~citympg)
summary(mod2) # r2adj = 0.4676

# Remove citympg

# Trial 2:
vif(lm(
  price ~ highwaympg + horsepower + enginesize + stroke + compressionratio + carlength + carheight + curbweight + carwidth + wheelbase))


grDevices::windows()
pairs(~price + carlength + curbweight + wheelbase)


# Curbweight, carlength, and wheelbase seem dependent?

mod3 <- lm(price~curbweight)
summary(mod3) # r2adj = 0.6962

mod4 <- lm(price~carwidth)
summary(mod4) # r2adj = 0.4638

mod5 <- lm(price~wheelbase)
summary(mod5) # r2adj = 0.3306

# remove carlength and wheelbase
vif(lm(
  price ~ highwaympg + horsepower + compressionratio  + enginesize + carheight + carwidth))

grDevices::windows()
vioplot(log(horsepower))
# Removed curbweight.

#Final toll: highwaympg, horsepower, enginesize, carwidth, comressionratio, carheight

grDevices::windows()
pairs(~price + highwaympg + enginesize + compressionratio + curbweight + stroke)

#----------------------------------------- Residuals --------------------------#
grDevices::windows()
vioplot(log(price))






#------------------------------------------------------------------------------#
#Here the same procedure is done again for new model. Here, covariates are
#roomtype, dist, number of reviews, and minimum nights (1 cat, 3 cont).
#The procedure is otherwise the same as before, completely.

#Final toll: highwaympg, horsepower, enginesize, carwidth, comressionratio, carheight

m <-
  lm(
    log(price) ~ drivewheel + carwidth + compressionratio +
      carheight + highwaympg + enginesize + log(horsepower),
    x = TRUE
  )

#removed fueltype and cylindernumber


X <- m$x  # extract the design matrix
X <- X[,-c(1)] # remove the intercept column just because regsubsets function below does not want it
X <- as.matrix(X)
y <- log(price)


library(leaps)
# columns 1 to 11 in xx are levels of categorical covariate. Let's force it in each model to fit
m <-
  regsubsets(
    X,
    y,
    int = T,
    nbest = 1000,
    nvmax = dim(X)[2],
    method = c("ex"),
    really.big = T,
    force.in = c(1,2)
  )
cleaps <- summary(m, matrix = T)
cleaps$which
# notice regsubsets is not really optimized to work with categorical covariates. For example it shows for the
# first fitted model that this one has 3 covariates (excluding the intercept), that is origineuropean, originjapanese
# and weight. But in reality these are only 2 covariates, corresponding to "origin" and "weight".

# also, notice that when we use force.in then regsubsets rearranges the order of covariates so that the categorical
# dummy variables appear first. WE MUST TAKE THIS INTO ACCOUNT! SO here is another hack:
# we reorder the columns according to the order found in cleaps$which
col.order <- c(colnames(cleaps$which))
X <- X[,col.order[-1]] # now the order of columns in X is consistent with the order in cleaps$which

Models <- cleaps$which

set.seed(123)  # set seed for reproducibility
K <- 5
n <- length(y)
ii <- sample(seq(1,n), n)  # vector of length n
foldsize <- floor(n/K)  
sizefold <- rep(foldsize,K)  # here each fold has size n/K
Prederrors <- matrix(0,dim(Models)[1],K)
iused <- 0  # initialize number of indeces used for testing


for (k in (1:K)) {
  itest <- ii[(iused + 1):(iused + sizefold[k])]
  itrain <- ii[-c((iused + 1):(iused + sizefold[k]))]
  iused <- iused + length(itest) 
  Prederrors[1,k] <- sum((y[itest] - mean(y[itrain]))^2) # intercept only 
  for (mm in (1:dim(Models)[1])) {  # loops through all model subsets 
    xtrain <- X[itrain,Models[mm,2:dim(Models)[2]]]  # extract training design matrix for model mm 
    xtrain <- cbind(rep(1, dim(xtrain)[1]), xtrain)  # add in the intercept column of ones since we now need to estimate also the intercept via least squares
    betahat <- solve(t(xtrain)%*%xtrain)%*%t(xtrain)%*%y[itrain]  # least squares formula for parameter estimates
    xtest <- X[itest,Models[mm,2:dim(Models)[2]]]  # extract test design matrix for model mm 
    xtest <- cbind(rep(1, dim(xtest)[1]), xtest)  # add in the intercept column of ones since we now need to make predictions
    ypred<-xtest%*%betahat
    Prederrors[mm,k]<-sum((y[itest]-ypred)^2) 
  }
}
PE <- apply(Prederrors,1,sum)/n  # final prediction errors.

# now we should rearrange the PE values according to increasing model complexity.
Models
# we see from above that, since the categorical covariate levels have been forced 
# we have that
# model 1 has complexity 2 (= categorical covar + weight)
# model 2 has complexity 2 (= categorical covar + horsepower)
# etc
#complexity <- c(rep(2,8),rep(3,28),rep(4,56),rep(5,70),rep(6,56),rep(7,28),rep(8,8),9) # lazy and pedestrian...you can also write  complexity <- c(rep(2,5),...)
complexity <- c(rep(2,6),rep(3,15),rep(4,20),rep(5,15),rep(6,6),7)

grDevices::windows()
plot(complexity,PE) #Possibly model with complexity = 4 is good?

# it seems that already with the smallest complexity we have a good model
# with 2 covariates (complexity=2) the first model has PE=18.47;
# with 3 covariates (complexity = 3) the sixth model has PE=17.51

# the first model has PE=18.47

PE[complexity == 5]


Models[22,] # Model 22 with drivewheel, carwidth, compressionratio, and log(horsepower)
#(Intercept) origineuropean originjapanese      cylinders   displacement     horsepower         weight   acceleration 
#       TRUE           TRUE           TRUE          FALSE          FALSE          FALSE           TRUE          FALSE 

# Weight is important

# the sixth model has PE=17.51
Models[42,] # or for complexity == 5 -> all but highwaymph and carheight
#(Intercept) origineuropean originjapanese      cylinders   displacement     horsepower         weight   acceleration 
#       TRUE           TRUE           TRUE          FALSE          FALSE           TRUE           TRUE          FALSE 

# So Horsepower is also important 

# Now let's compare predictions using the first with the observed data from the test set
chosenmod <- lm(y[itrain]~X[itrain,Models[22,-1]==T])  # fit the training data with model #1
betachosen <- chosenmod$coefficients  # parameter estimates from training data
# select "active variables" in TEST data, corresponding to model #1
designchosen <- X[itest,Models[22,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set

grDevices::windows()
plot(y[itest],ypred)  # compare predictions with test responses
abline(0,1)   

summary(lm(log(price)~drivewheel+log(horsepower)+carwidth+compressionratio))  # Model #1 --> Rsquared = 0.70 on the FULL data

# Now let's compare predictions using the 6th model (horsepower+weight) with the observed test responses
chosenmod <- lm(y[itrain]~X[itrain,Models[42,-1]==T]) 
betachosen <- chosenmod$coefficients  # parameter estimates from our model on training data
# select "active variables" in TEST data, corresponding to model #6
designchosen <- X[itest,Models[42,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set

grDevices::windows()
plot(y[itest],ypred)  # compare predictions with test responses
abline(0,1)   # not bad!

#summary(lm(mpg~origin+horsepower+weight)) # Model #6 -->  Rsquared = 0.72 on the FULL data


#---------------------- Redo but with cylinder instead ------------------------#
#Here the same procedure is done again for new model. Here, covariates are
#roomtype, dist, number of reviews, and minimum nights (1 cat, 3 cont).
#The procedure is otherwise the same as before, completely.

#CON: Cannot set size for training and test data - not even split...
#Instead, package contains other measures (AIC, Cp, etc).
#Hence, do not get model through out-of-sample pred

modols<-lm(log(price)~ cylindernumber + fueltype + drivewheel + carwidth + compressionratio +
             carheight + highwaympg + enginesize + log(horsepower))
out<-ols_step_all_possible(modols) #regression of all subsets.


grDevices::windows() 
plot(out) #Decide model after Mallow's Cp.
#Look in out for the best model: 
out


# Now let's compare predictions using such model with the observed data from the test set
chosenmod <- lm(yy~xx[,cleaps$which[6,-1]==T])  # fit the training data with our favorite model  (model #6)
betachosen <- chosenmod$coefficients  # parameter estimates from our favorite model on training data
# select "active variables" in TEST data, corresponding to model #6 
designchosen <- xxt[,cleaps$which[6,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(xxt)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set
plot(yyt,ypred)  # compare predictions with test responses
abline(0,1)   # not bad!

#--------------------------- Confidence interval ------------------------------#
mymodel2<-lm(log(price)~drivewheel + compressionratio + log(horsepower) + carwidth,x=TRUE)
summary(mymodel2) # Multiple R-squared:  0.8563
# 95% confidence interval for beta1 "by hand":



# let's first compute the residual standard error: this can be found from summary(mymodel3)$sigma
# that is s = 14.9
s = summary(mymodel2)$sigma  # also found via sqrt(sum(residuals(mymodel3)^2)/(10-3)) and summary(mymodel3)$sigma

# now we need the inverse of X'X
invXtX = summary(mymodel2)$cov.unscaled # this is inv(X'X)
# find inverse "by hand"
X <- mymodel2$x  # get design matrix
solve(t(X)%*%X)  # invert X'X. Looks the same as invXtX. Great!

# therefore the standard error for beta1 is
se_beta1 = sqrt(s^2*invXtX[2,2])  # compare with summary(mymodel2), differences due to rounding since I hard-coded s=14.9 without further decimals
# the 95% confidence interval for beta1 is
36293.78 +c(1,-1)*qt(0.025,10-3)*se_beta1
# or, see the second line of
confint(mymodel2)  # differences due to rounding since I have hard-coded s=14.9 without further decimals

#---------------------- Leverage ----------------------------------------------#
# computation of leverage by hand
#X <- matrix(c(rep(1,nrow(w)),w$x),nrow=nrow(w))
#xtx <- t(X) %*% X
#invxtx <- solve(xtx)
#P <- X %*% invXtX %*% t(X)
#v <- diag(P)  # same as v <- hatvalues(mod)

V <- hatvalues(mymodel2)


infl <- lm.influence(mymodel2) # will calculate basic influence measures
# type ?lm.influence for more info
s_i <- infl$sigma #a vector whose i-th element contains the estimate of the residual standard deviation obtained when the i-th case is dropped from the regression        
# v <- infl$hat
sum <- summary(mymodel2)
#plot(s_i,ylim=c(0,6))  # <--this work just as well as the code below
grDevices::windows()
plot(s_i,xlab="i",ylab="s_(i)",
     main="s_(i)")
#points(Iout,s_i[Iout],col="red",pch=19)
#points(Iinf,s_i[Iinf],col="green",pch=19)
abline(h=sum$sigma)

#----------------- studentised residuals --------------------------------------#
# VERSION WITHOUT BONFERRONI CORRECTION (SEE BELOW FOR A FIX)
r_stud <- rstudent(mymodel2)

grDevices::windows()
plot(r_stud,xlab="i",ylab="r*_i",
     main="stud. residuals, +/- 2")
#points(Iout,r_stud[Iout],col="red",pch=19)
#points(Iinf,r_stud[Iinf],col="green",pch=19)
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")

# VERSION WITH BONFERRONI CORRECTION FOR MULTIPLE COMPARISON
grDevices::windows()
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
     main="stud. residuals, +/- T-Bonferroni corrected")
#points(Iout,r_stud[Iout],col="red",pch=19)
#points(Iinf,r_stud[Iinf],col="green",pch=19)
abline(h=0)
alpha_original = 0.05
n <- 205#dim(cars)[1]  # n=50 observations
alpha_bonferroni = alpha_original/n
t_bonferroni = qt(alpha_bonferroni/2,n-1-2)  # quantile from a Student's t with n-1-p degrees of freedom at probability level alpha/(2*n)
abline(h=t_bonferroni,col="red")
abline(h=-t_bonferroni,col="red")

#---------------------- Cook's distance ---------------------------------------#
D<-cooks.distance(mymodel2)
grDevices::windows()
plot(D,ylim=c(0,1.2),xlab="i",ylab="D_i",
     main="Cook's distance with D=4/n and D=1")
#points(Iout,D[Iout],col="red",pch=19)
#points(Iinf,D[Iinf],col="green",pch=19)
abline(h=4/nrow(w))
abline(h=1)
## Standardized residuals:
# r_std <- rstandard(mod)
# r_std <- res/(sum$sigma*sqrt(1-v))
# D <- r_std^2/2*v/(1-v)

#------------------ DFBETAs ---------------------------------------------------#
## Standardized change in beta-estimates (DFBETAS):
# you may use dfbetas(mod)
beta0_i <- infl$coefficients[,1] # gives the numerator for the DFBETA of beta0, type "?lm.influence" for details
beta1_i <- infl$coefficients[,2] # gives the numerator for the DFBETA of beta1, type "?lm.influence" for details
dfbeta_0=beta0_i/s_i/sqrt(invXtX[1,1])
dfbeta_1=beta1_i/s_i/sqrt(invXtX[2,2])

grDevices::windows()
plot(dfbeta_0,ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
#points(Iout,dfbeta_0[Iout],col="red",pch=19)
#points(Iinf,dfbeta_0[Iinf],col="green",pch=19)
abline(h=0)
abline(h=2/sqrt(nrow(w)),col="red")
abline(h=-2/sqrt(nrow(w)),col="red")

grDevices::windows()
plot(dfbeta_1,ylim=c(-1,1),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
points(Iout,dfbeta_1[Iout],col="red",pch=19)
points(Iinf,dfbeta_1[Iinf],col="green",pch=19)
abline(h=0)
abline(h=2/sqrt(nrow(w)),col="red")
abline(h=-2/sqrt(nrow(w)),col="red")
