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
relevel(cars$fueltype, ref = "gas")

# Aspiration
cars$aspiration <- factor(aspiration)

# Number of doors (excluding trunk dooor)
cars$doornumber <- factor(doornumber)

# Type of car body
cars$carbody <- factor(carbody)
relevel(cars$carbody, ref = "hatchback")

# Type of wheel drive
cars$drivewheel <- factor(drivewheel)
relevel(cars$drivewheel , ref ="rwd")

# Engine location
cars$enginelocation <- factor(enginelocation)

# Engine type
cars$enginetype <- factor(enginetype)
relevel(cars$enginetype, ref = "ohc")

# Number of cylinders
cars$cylindernumber <- factor(cylindernumber)
relevel(cars$cylindernumber , ref ="four")

# Fuel system
cars$fuelsystem <- factor(fuelsystem)
relevel(cars$fuelsystem , ref ="2bbl")

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
cars <- subset(cars, enginelocation == "front")

# Only small amount of cars with aspiration == turbo,
# Decided to focus on cars with standard aspiration:
cars <- subset(cars, aspiration == "std")

grDevices::windows()
vioplot(compressionratio, main="Violin plot for compression ratio", ylab = "Compression ratio")
# After lookin at the violin plot for compression ratio, I decided to remove
# all cars with ratio over 15.
cars <- subset(cars, compressionratio <= 15)

# Possibly also only look at gas cars? It seems that all diesel cars have
# already been excluded??
grDevices::windows()
plot(fueltype,price)

# It seems that the engine size might be grouped too?
grDevices::windows()
vioplot(enginesize, main="Violin plot for engine size", ylab = "Compression ratio")

cars <- subset(cars, enginesize <= 200)



cars <- subset(cars, fueltype=="gas")


# Suspect that id is not important for price:
cars <- subset(cars, select = -c(car_ID))

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
    log(price) ~ drivewheel + cylindernumber + carwidth + compressionratio +
      carheight + highwaympg + enginesize + log(horsepower),
    x = TRUE
  )
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
    force.in = c(1,2,3,4,5,6)
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
complexity <- c(rep(2,8),rep(3,28),rep(4,56),rep(5,70),rep(6,56),rep(7,28),rep(8,8),9) # lazy and pedestrian...you can also write  complexity <- c(rep(2,5),...)


grDevices::windows()
plot(complexity,PE)

# it seems that already with the smallest complexity we have a good model
# with 2 covariates (complexity=2) the first model has PE=18.47;
# with 3 covariates (complexity = 3) the sixth model has PE=17.51

# the first model has PE=18.47
Models[1,]
#(Intercept) origineuropean originjapanese      cylinders   displacement     horsepower         weight   acceleration 
#       TRUE           TRUE           TRUE          FALSE          FALSE          FALSE           TRUE          FALSE 

# Weight is important

# the sixth model has PE=17.51
Models[6,]
#(Intercept) origineuropean originjapanese      cylinders   displacement     horsepower         weight   acceleration 
#       TRUE           TRUE           TRUE          FALSE          FALSE           TRUE           TRUE          FALSE 

# So Horsepower is also important 

# Now let's compare predictions using the first with the observed data from the test set
chosenmod <- lm(y[itrain]~X[itrain,Models[1,-1]==T])  # fit the training data with model #1
betachosen <- chosenmod$coefficients  # parameter estimates from training data
# select "active variables" in TEST data, corresponding to model #1
designchosen <- X[itest,Models[1,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set
plot(y[itest],ypred)  # compare predictions with test responses
abline(0,1)   

summary(lm(mpg~origin+weight))  # Model #1 --> Rsquared = 0.70 on the FULL data

# Now let's compare predictions using the 6th model (horsepower+weight) with the observed test responses
chosenmod <- lm(y[itrain]~X[itrain,Models[6,-1]==T]) 
betachosen <- chosenmod$coefficients  # parameter estimates from our model on training data
# select "active variables" in TEST data, corresponding to model #6
designchosen <- X[itest,Models[6,-1]==T]
#add a column of ones for the intercept
designchosen <- cbind(rep(1,dim(designchosen)[1]),designchosen)
ypred <- designchosen%*%betachosen  # obtain predictions using the covariates from the test set
plot(y[itest],ypred)  # compare predictions with test responses
abline(0,1)   # not bad!

summary(lm(mpg~origin+horsepower+weight)) # Model #6 -->  Rsquared = 0.72 on the FULL data
